package model

import domain._
import model.ApplicationModel.{PoolSource, PoolSourceArchive, PoolSourceDirectory}
import monix.execution.{Scheduler => MonixScheduler}

import scala.util.{Success, Try}


trait PoolResourceProviderFactory {
  def getProvider(source: PoolSource): Try[PoolResourceProvider]
}

class PoolResourceProviderFactoryImpl(
  orderDocsParser: OrderDocumentsParser, orderDocsCacheSize: Int
)(implicit scheduler: MonixScheduler) extends PoolResourceProviderFactory {
  override def getProvider(source: PoolSource): Try[PoolResourceProvider] = {
    source match {
      case PoolSourceArchive(archivePath) => Success(new PoolResourceProviderTarGzImpl(archivePath, orderDocsParser, orderDocsCacheSize))
      case PoolSourceDirectory(path) => Success(new PoolResourceProviderDirectoryImpl(path, orderDocsParser))
    }
  }
}
