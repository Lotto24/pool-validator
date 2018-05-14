package domain

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import domain.products.GamingProductOrder
import monix.execution.Scheduler.Implicits.global
import org.apache.commons.io.{FileUtils, FilenameUtils}
import org.scalatest._
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.reflect.{ClassTag, classTag}
import scala.util.Success

/** `PoolResourceProviderSpec` defines some common tests that are executed in `PoolResourceProviderTarGzImplSpec`
  * and `PoolResourceProviderDirectoryImplSpec`. */
trait PoolResourceProviderSpec extends FunSpecLike with Matchers with Inside with ScalaFutures with Eventually {

  protected val workingDir = new File(System.getProperty("user.dir"))
  protected val dummyPath = new File("some/dummyFile").toPath

  protected val orderDocsParser = new OrderDocumentsParserPlayImpl(CompositeProductOrderFactory.allProductsOrderFactory)

  implicit override val patienceConfig = PatienceConfig(timeout = 3.seconds, interval = 10.millis)

  protected val logger = LoggerFactory.getLogger(getClass)

  protected val archiveFile: File = testResourceFile("src/test/resources/testdata_qa/mmls-2018-05-08.tgz")

  def withPoolArchive(file: File)(test: File => Unit): Unit = {
    file.exists() shouldBe true
    file.canRead shouldBe true
    test(file)
  }

  def createProvider(poolArchive: File): PoolResourceProvider

  //like `inside`, but without dumping the `data` on failure
  def doWith[T](data: T)(f: T => Unit): Unit = f(data)

  describe("Iterating OrderDocs") {

    it("should work with a valid archive & read the right files") {

      withPoolArchive(archiveFile) { file =>
        val provider = createProvider(file)

        whenReady(provider.getOrderDocsObservable().toListL.runAsync.map(_.sortBy(_.get.orderId))) { orderDocs =>
          orderDocs.size shouldBe 5

          doWith(orderDocs(0)) { case (Success(orderDocs)) =>
            orderDocs.orderId shouldBe "4tS8YcGABW5n1lYWEoQgrY8zqkaprTUcYPsbSR2p1kY"
            orderDocs.orderId shouldBe orderDocs.order.get.orderId

            orderDocs.order.get.metaData.retailerOrderReference shouldBe "90f80cd4-45f4-4efa-a2e2-c13791755249"

            orderDocs.orderResult.get.orderDigest shouldBe bytes("sha256=4tS8YcGABW5n1lYWEoQgrY8zqkaprTUcYPsbSR2p1kY=")

            orderDocs.orderResultSignature.get.signature shouldBe bytes("ZxCa/H3AdjvaWmUMbFl20GfwdFG39X01J6CpskzJMNWXPdgxQ97LXu7Vgr2L9dqF7BydqkBKJLdA0ka2por2G+5srooSRZnsAE05KfHdZL9Q/1EtHiUxkcfb7yLosPyHQ0sfOGi8S1o0xNMYVdsuq03tQKRnX2LN/JccadvtU78=")
          }

          doWith(orderDocs(1)) { case (Success(orderDocs)) =>
            orderDocs.orderId shouldBe "7GPM4g6PxTDKRL9uygqDNkiPBf2_94dGSxRbEnb49NY"
            orderDocs.orderId shouldBe orderDocs.order.get.orderId

            orderDocs.order.get.metaData.retailerOrderReference shouldBe "c98e398e-944d-4fa2-b1ad-6c2aa31d9971"

            orderDocs.orderResult.get.orderDigest shouldBe bytes("sha256=7GPM4g6PxTDKRL9uygqDNkiPBf2/94dGSxRbEnb49NY=")

            orderDocs.orderResultSignature.get.signature shouldBe bytes("vTO9L87SND+kgFWkQgzUmNT1xw/1xQ6QTzTigQ04CmV0EeQzPJjeYfU1EdKQJ8tiZDVpgbcCiyDgmfKT1VXuDhokufzvpC6CMgDdPNopDnlv9w+GKP1O4uSjT3bBsUhE1ibo8E9/TDFrBnTggUTxUGFsWXsN4osB3kerbdzLf/Y=")
          }

          doWith(orderDocs(4)) { case (Success(orderDocs)) =>
            orderDocs.orderId shouldBe "bBzP2JElc6rUXT9RlJKzzs29rh5I6DP-L_dMLpnoZGU"
            orderDocs.orderId shouldBe orderDocs.order.get.orderId

            orderDocs.order.get.metaData.retailerOrderReference shouldBe "31f92094-f96a-403d-8965-f348dfe258bd"

            orderDocs.orderResult.get.orderDigest shouldBe bytes("sha256=bBzP2JElc6rUXT9RlJKzzs29rh5I6DP+L/dMLpnoZGU=")

            orderDocs.orderResultSignature.get.signature shouldBe bytes("D+SxnK2RN+0ZlPJb7zXlUBymuG4hcrAuYP/BP7+n/YxT80MTAyguYJAH/KC5HcSTj/RymOt9NgzkrYw4WgyXKpLB8Vb3VHWOvInBJFxVqESTjAep5hF7CUTdJuWdIM2raHaTHwVy1z2u1nSNp+bmgW6959gIpbKtL7htwC36ux8=")
          }
        }
      }
    }
  }

  describe("Get order docs for a certain orderId") {

    describe("getOrder") {
      it("should work for valid orderIds") {
        withPoolArchive(archiveFile) { file =>
          val provider = createProvider(file)
          doWith("4tS8YcGABW5n1lYWEoQgrY8zqkaprTUcYPsbSR2p1kY") { orderId =>
            provider.getOrder(orderId).get.orderId shouldBe orderId
          }
          doWith("7GPM4g6PxTDKRL9uygqDNkiPBf2_94dGSxRbEnb49NY") { orderId =>
            provider.getOrder(orderId).get.orderId shouldBe orderId
          }
          doWith("bBzP2JElc6rUXT9RlJKzzs29rh5I6DP-L_dMLpnoZGU") { orderId =>
            provider.getOrder(orderId).get.orderId shouldBe orderId
          }
        }
      }

      it("should fail for invalid orderIds") {
        withPoolArchive(archiveFile) { file =>
          val provider = createProvider(file)
          doWith("nonExistingOrderId") { orderId =>
            provider.getOrder(orderId).failed.get.getMessage should include("does not exist")
          }
        }
      }
    }

    describe("getOrderResult") {
      it("should work for valid orderIds") {
        withPoolArchive(archiveFile) { file =>
          val provider = createProvider(file)

          provider.getOrderResult("4tS8YcGABW5n1lYWEoQgrY8zqkaprTUcYPsbSR2p1kY").get
            .retailerOrderReference shouldBe "90f80cd4-45f4-4efa-a2e2-c13791755249"

          provider.getOrderResult("7GPM4g6PxTDKRL9uygqDNkiPBf2_94dGSxRbEnb49NY").get
            .retailerOrderReference shouldBe "c98e398e-944d-4fa2-b1ad-6c2aa31d9971"

          provider.getOrderResult("bBzP2JElc6rUXT9RlJKzzs29rh5I6DP-L_dMLpnoZGU").get
            .retailerOrderReference shouldBe "31f92094-f96a-403d-8965-f348dfe258bd"
        }
      }

      it("should fail for invalid orderIds") {
        withPoolArchive(archiveFile) { file =>
          val provider = createProvider(file)
          doWith("nonExistingOrderId") { orderId =>
            provider.getOrderResult(orderId).failed.get.getMessage should include("does not exist")
          }
        }
      }
    }
  }

  protected def testResourceFile(resourcePath: String): File = new File(new File(System.getProperty("user.dir")), resourcePath)

  protected def checkProductOrder[T <: GamingProductOrder : ClassTag](order: GamingProductOrder)(f: T => Unit): Unit = {
    withClue(s"check ${classTag[T].runtimeClass.getSimpleName}:$order") {
      f(order.asInstanceOf[T])
    }
  }

  protected def bytes(data: String): Vector[Byte] = data.getBytes(UTF_8).toVector

}


class PoolResourceProviderTarGzImplSpec extends PoolResourceProviderSpec {

  override def createProvider(poolArchive: File): PoolResourceProviderTarGzImpl = {
    new PoolResourceProviderTarGzImpl(poolArchive.toPath, orderDocsParser, orderDocsCacheSize = 10)
  }

  it("should fail for directory paths") {
    val provider = createProvider(
      new File(System.getProperty("user.dir")).toPath.resolve("src/test/resources/testdata_qa/mmls-2018-05-08").toFile
    )
    whenReady(provider.getOrderDocsObservable().toListL.runAsync.failed) { t =>
      t.getMessage should include("mmls-2018-05-08")
    }
  }
}


class PoolResourceProviderDirectoryImplSpec extends PoolResourceProviderSpec {

  override def createProvider(poolArchive: File): PoolResourceProvider = {
    new PoolResourceProviderDirectoryImpl(poolArchive.toPath, orderDocsParser)
  }

  override def withPoolArchive(file: File)(test: File => Unit): Unit = {
    whenReady(extractTarGzArchiveToTmpDir(file)) { file =>
      try {
        test(file)
      } finally {
        logger.info(s"deleting tmp dir $file")
        FileUtils.deleteDirectory(file)
      }
    }
  }

  it("should fail for non-directory paths") {
    val provider = createProvider(
      new File(System.getProperty("user.dir")).toPath.resolve("src/test/resources/testdata_qa/mmls-2018-05-08.tgz").toFile
    )
    whenReady(provider.getOrderDocsObservable().toListL.runAsync.failed) { t =>
      t.getMessage should include("mmls-2018-05-08.tgz is a File (directory expected)")
    }
  }

  private def extractTarGzArchiveToTmpDir(file: File): Future[File] = {
    file.exists() shouldBe true
    file.canRead shouldBe true
    val fileNameWithoutSuffix = FilenameUtils.removeExtension(file.getName)
    val extractToDir = Files.createTempDirectory(fileNameWithoutSuffix)
    logger.info(s"extracting archive $file to $extractToDir..")
    ArchiveReader.extractArchive(file, destDir = extractToDir.toFile).runAsync.andThen { case Success(dir) =>
      logger.info(s"working with archive $dir")
    }
  }
}
