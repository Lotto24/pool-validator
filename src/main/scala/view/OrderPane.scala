package view


import java.time.format.{DateTimeFormatter, FormatStyle}

import javafx.beans.binding.Bindings
import javafx.scene.control._
import javafx.scene.layout._
import domain.products.GamingProduct.gamingProductIdFromURI
import domain.products.amls.{AmlsBet, AmlsGamingProductOrder, AmlsParticipationPools}
import domain.products.aols.{AolsBet, AolsGamingProductOrder, AolsParticipationPools}
import domain.products.apls.{AplsBet, AplsGamingProductOrder, AplsParticipationPools}
import domain.products.asls.{AslsBet, AslsGamingProductOrder, AslsParticipationPools}
import domain.products.awls.{AwlsBet, AwlsGamingProductOrder, AwlsParticipationPools}
import domain.products.c4ls.{C4lsBet, C4lsGamingProductOrder, C4lsParticipationPools}
import domain.products.ejs.{EjsBet, EjsGamingProductOrder, EjsParticipationPools}
import domain.products.ems.{EmsBet, EmsGamingProductOrder, EmsParticipationPools}
import domain.products.emsplus.{EmsPlusBet, EmsPlusGamingProductOrder, EmsPlusParticipationPools}
import domain.products.fls.{FlsBet, FlsGamingProductOrder, FlsParticipationPools}
import domain.products.gls.{GlsBet, GlsGamingProductOrder, GlsParticipationPools}
import domain.products.glss.{GlsSBet, GlsSGamingProductOrder, GlsSParticipationPools}
import domain.products.irishraffle.{IrishRaffleBet, IrishRaffleGamingProductOrder, IrishRaffleParticipationPools}
import domain.products.irls.p1.{IrlsP1GamingProductOrder, IrlsP1ParticipationPools}
import domain.products.irls.p2.{IrlsP2GamingProductOrder, IrlsP2ParticipationPools}
import domain.products.irls.{IrlsBet, IrlsGamingProductOrder, IrlsParticipationPools}
import domain.products.keno.{KenoBet, KenoGamingProductOrder, KenoParticipationPools}
import domain.products.mmls.{MmlsBet, MmlsGamingProductOrder, MmlsParticipationPools}
import domain.products.pls.{PlsBet, PlsGamingProductOrder, PlsParticipationPools}
import domain.products.plus5.{Plus5Bet, Plus5GamingProductOrder, Plus5ParticipationPools}
import domain.products.s6.{S6Bet, S6GamingProductOrder, S6ParticipationPools}
import domain.products.s77.{S77Bet, S77GamingProductOrder, S77ParticipationPools}
import domain.products.sls.{SlsBet, SlsGamingProductOrder, SlsParticipationPools}
import domain.products.ukls.{UklsBet, UklsGamingProductOrder, UklsParticipationPools}
import domain.products.uktbls.{UktblsBet, UktblsGamingProductOrder, UktblsParticipationPools}
import domain.products.uspbls.{UspblsBet, UspblsGamingProductOrder, UspblsParticipationPools}
import domain.products.xmasl.{XmaslBet, XmaslGamingProductOrder, XmaslParticipationPools}
import domain.products.{Bet, GamingProductOrder, ParticipationPools, ParticipationPoolsMultiplyDays}
import play.api.libs.json.Json
import view.DetailView._
import view.GenericOrderPane.{dateFormat => _, _}
import view.JfxImplicits._
import view.controls.TextAreaWithAutoHeight
import view.impl.StructureElements.{SectionSeparator, VSpacer}

import scala.collection.immutable.HashMap
import scala.reflect.{ClassTag, _}
import scalafx.Includes._

/**
  * Basic trait `GamingProductOrder`-visualization `Pane`s.
  * */
trait OrderPane extends Pane {

  def setOrder(order: GamingProductOrder): this.type
  
}

/**
  * `GenericOrderPane` visualizes a `GamingProductOrder` using a `TableView` for the `Bet`s and 
  * multiple structured key-value panes for other order-related data.
  * @param betNumberSpecs enables to define the kind of numbers for the `GamingProductOrder` (especially the display-name and an extractor-function).  
  * @param productCssClass allows product-specific adjustment of the appearance. 
  * */
class GenericOrderPane[O <: GamingProductOrder, P <: ParticipationPools : ClassTag, B <: Bet](
  betNumberSpecs: Seq[GenericOrderPane.BetNumbersSpec[B]],
  productCssClass : String
) extends VBox with OrderPane {
  
  val isMultiDrawDaysPool : Boolean = classOf[ParticipationPoolsMultiplyDays].isAssignableFrom(classTag[P].runtimeClass)

  //GamingProductOrder fields
  protected val kvl_productId = new KeyValuePair("Product-ID", level=3, cssClass = "orderinfo")
  protected val kvl_variant = new KeyValuePair("Variant", level=3, cssClass = "orderinfo")

  protected val tableViewBets = new TableView[B]

  val poolSpecsPane : PoolSpecsPane = new PoolSpecsPaneImpl(isMultiDrawDaysPool)

  locally{
    tableViewBets.getStyleClass += "bets"

    val heightBinding = tableViewBets.fixedCellSizeProperty().multiply(Bindings.size(tableViewBets.getItems()).add(1.16))
    tableViewBets.prefHeightProperty().bind(heightBinding)
    tableViewBets.minHeightProperty().bind(heightBinding)
    tableViewBets.maxHeightProperty().bind(heightBinding)

    tableViewBets.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY)

    createTableColumns()

    tableViewBets.getColumns.foreach{ col =>
      col.setResizable(false)
      col.impl_setReorderable(false)
      col.setSortable(false)
    }

    getChildren.addAll(
      new SectionSeparator("Gaming product order", level = 3),
      kvl_productId,
      kvl_variant,
      new SectionSeparator("Bets", level = 3),
      tableViewBets,
      new VSpacer(),
      new SectionSeparator("Participation pools", level = 3),
      poolSpecsPane,
      new VSpacer()
    )

    //'product_number-types_x' is used to adjust the TableView-style (e.g. to suppress an empty column for Products with only one kind of numbers)
    getStyleClass.addAll("generic-order-pane", productCssClass, s"product_number-types_${betNumberSpecs.size}")
  }

  protected def createTableColumns(): Unit = {
    betNumberSpecs.foreach{ betNumberSpec =>
      tableViewBets.getColumns.add(new TableColumn[B, String] {
        getStyleClass += betNumberSpec.cssClass
        setText(betNumberSpec.name)
        this.addCellValuePojoSource{cdf =>
          betNumberSpec.extractor(cdf.getValue).toSeq.sorted.mkString(", ")
        }
      })
    }
  }
    
  override def setOrder(order: GamingProductOrder): this.type = {
    this.kvl_productId.setValue(gamingProductIdFromURI(order.productURI))
    this.kvl_variant.setValue(order.variant.getOrElse(""))
    tableViewBets.getItems.setAll(order.bets.asInstanceOf[Seq[B]]: _*)
    poolSpecsPane.setValue(order.participationPools)
    this
  }
}


object GenericOrderPane {
  
  case class BetNumbersSpec[B](name: String, cssClass: String, extractor: B => Iterable[Int])

  trait PoolSpecsPane extends Pane {
    def setValue(pools: ParticipationPools): Unit
  }
  
  /**
    * Visualizes a `ParticipationPools`.
    * */
  class PoolSpecsPaneImpl(poolWithMultipleDays: Boolean) extends VBox with PoolSpecsPane {
    private val kvl_firstDate = new KeyValuePair("First date", level=3, cssClass = "partpools")
    private val kvl_drawCount = new KeyValuePair("Draw count", level=3, cssClass = "partpools")
    private val kvl_drawDays = new KeyValuePair("Draw days", level=3, cssClass = "partpools")
    
    locally{
      getChildren.addAll(kvl_firstDate, kvl_drawCount)
      if(poolWithMultipleDays) {
        getChildren.add(kvl_drawDays)
      }
    }

    override def setValue(pools: ParticipationPools): Unit = {
      kvl_firstDate.setValue(dateFormat.format(pools.firstDate))
      kvl_drawCount.setValue(pools.drawCount.toString)
      pools match {
        case p : ParticipationPoolsMultiplyDays =>
          kvl_drawDays.setValue(p.drawDays.mkString(", ").toString)
        case _ => // do nothing
      }
    }
  }

  private val dateFormat = DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM)
}


/** Displays the raw data of a `GamingProductOrder`. */
class DefaultOrderPane extends VBox with OrderPane {
  val taOrderRawData = new TextAreaWithAutoHeight
  locally{
    getStyleClass.add("default-order-pane")
    taOrderRawData.setEditable(false)
    getChildren.addAll(taOrderRawData)
  }

  override def setOrder(order: GamingProductOrder): this.type = {
    taOrderRawData.setText(Json.prettyPrint(order.json))
    this
  }
}


object OrderPaneFactory {
  
  /**
    * Creates a specific `OrderPane` for the given `GamingProductOrder` if available, a `DefaultOrderPane` otherwise. 
    * */
  def createOrderPane(order: GamingProductOrder): OrderPane = {
    factoryMethodsMap.get(order.getClass) match {
      case Some(factoryMethod) => factoryMethod.apply()
      case _ => new DefaultOrderPane
    }
  }

  private val factoryMethodsMap = HashMap[Class[_<: GamingProductOrder], () => OrderPane](

    classOf[AmlsGamingProductOrder] -> { () => 
      new GenericOrderPane[AmlsGamingProductOrder, AmlsParticipationPools, AmlsBet](
        Seq(BetNumbersSpec[AmlsBet](name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "amls"
      )
    },
    classOf[AolsGamingProductOrder] -> { () =>
      new GenericOrderPane[AolsGamingProductOrder, AolsParticipationPools, AolsBet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "aols"
      )
    },
    classOf[AplsGamingProductOrder] -> { () =>
      new GenericOrderPane[AplsGamingProductOrder, AplsParticipationPools, AplsBet](
        Seq(
          BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers),
          BetNumbersSpec(name = "Powerball", cssClass = "extranumbers", x => Seq(x.powerball))
        ), productCssClass = "apls")
    },
    classOf[AslsGamingProductOrder] -> { () =>
      new GenericOrderPane[AslsGamingProductOrder, AslsParticipationPools, AslsBet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "asls"
      )
    },
    classOf[AwlsGamingProductOrder] -> { () =>
      new GenericOrderPane[AwlsGamingProductOrder, AwlsParticipationPools, AwlsBet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "awls"
      )
    },
    classOf[C4lsGamingProductOrder] -> { () => 
      new GenericOrderPane[C4lsGamingProductOrder, C4lsParticipationPools, C4lsBet](
        Seq(
          BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers),
          BetNumbersSpec(name = "Cashball", cssClass = "extranumbers", x => Seq(x.cashBall))
        ), productCssClass = "c4ls"
      )
    },    
    classOf[EjsGamingProductOrder] -> { () =>
      new GenericOrderPane[EjsGamingProductOrder, EjsParticipationPools, EjsBet](
        Seq(
          BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers),
          BetNumbersSpec(name = "Euronumbers", cssClass = "extranumbers", _.euroNumbers)
        ), productCssClass = "ejs")
    },
    classOf[EmsGamingProductOrder] -> { () =>
      new GenericOrderPane[EmsGamingProductOrder, EmsParticipationPools, EmsBet](
        Seq(
          BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers),
          BetNumbersSpec(name = "Starnumbers", cssClass = "extranumbers", _.starnumbers)
        ), productCssClass = "ems")
    },
    classOf[EmsPlusGamingProductOrder] -> { () =>
      new GenericOrderPane[EmsPlusGamingProductOrder, EmsPlusParticipationPools, EmsPlusBet](
        Seq(BetNumbersSpec(name="Numbers", cssClass="numbers", _.numbers)), productCssClass = "emsplus"
      )
    },
    classOf[FlsGamingProductOrder] -> { () =>
      new GenericOrderPane[FlsGamingProductOrder, FlsParticipationPools, FlsBet](
        Seq(
          BetNumbersSpec(name="Numbers", cssClass="numbers", _.numbers),
          BetNumbersSpec(name="Chancenumber", cssClass="extranumbers", x => Seq(x.chancenumber))
        ), productCssClass = "fls"
      )
    },
    classOf[GlsGamingProductOrder] -> { () =>
      new GenericOrderPane[GlsGamingProductOrder, GlsParticipationPools, GlsBet](
        Seq(
          BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers),
          BetNumbersSpec(name = "Supernumber", cssClass = "extranumbers", x => Seq(x.supernumber))
        ), productCssClass = "gls")
    },
    classOf[GlsSGamingProductOrder] -> { () =>
      new GenericOrderPane[GlsSGamingProductOrder, GlsSParticipationPools, GlsSBet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "glss"
      )
    },
    classOf[IrishRaffleGamingProductOrder] -> { () =>
      new GenericOrderPane[IrishRaffleGamingProductOrder, IrishRaffleParticipationPools, IrishRaffleBet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "irishraffle"
      )
    },
    classOf[IrlsGamingProductOrder] -> { () =>
      new GenericOrderPane[IrlsGamingProductOrder, IrlsParticipationPools, IrlsBet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "irls"
      )
    },
    classOf[IrlsP1GamingProductOrder] -> { () =>
      new GenericOrderPane[IrlsP1GamingProductOrder, IrlsP1ParticipationPools, IrlsBet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "irls_p1"
      )
    },
    classOf[IrlsP2GamingProductOrder] -> { () =>
      new GenericOrderPane[IrlsP2GamingProductOrder, IrlsP2ParticipationPools, IrlsBet](
        Seq(BetNumbersSpec(name="Numbers", cssClass="numbers", _.numbers)), productCssClass = "irls_p2"
      )
    },
    classOf[KenoGamingProductOrder] -> { () =>
      new GenericOrderPane[KenoGamingProductOrder, KenoParticipationPools, KenoBet](
        Seq(
          BetNumbersSpec(name="Numbers", cssClass="numbers", _.numbers),
          BetNumbersSpec(name="Stake", cssClass="extranumbers", x => Seq(x.stake))
        ), productCssClass = "keno"
      )
    },    
    classOf[MmlsGamingProductOrder] -> { () =>
      new GenericOrderPane[MmlsGamingProductOrder, MmlsParticipationPools, MmlsBet](
        Seq(
          BetNumbersSpec(name="Numbers", cssClass="numbers", _.numbers),
          BetNumbersSpec(name="Megaball", cssClass="extranumbers", x => Seq(x.megaBall))
        ), productCssClass = "mmls"
      )
    },    
    classOf[PlsGamingProductOrder] -> { () =>
      new GenericOrderPane[PlsGamingProductOrder, PlsParticipationPools, PlsBet](
        Seq(BetNumbersSpec(name="Numbers", cssClass="numbers", _.numbers)), productCssClass = "pls"
      )
    },
    classOf[Plus5GamingProductOrder] -> { () =>
      new GenericOrderPane[Plus5GamingProductOrder, Plus5ParticipationPools, Plus5Bet](
        Seq(BetNumbersSpec(name="Numbers", cssClass="numbers", _.numbers)), productCssClass = "plus5"
      )
    },
    classOf[S6GamingProductOrder] -> { () =>
      new GenericOrderPane[S6GamingProductOrder, S6ParticipationPools, S6Bet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "s6"
      )
    },
    classOf[S77GamingProductOrder] -> { () =>
      new GenericOrderPane[S77GamingProductOrder, S77ParticipationPools, S77Bet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "s77"
      )
    },
    classOf[SlsGamingProductOrder] -> { () =>
      new GenericOrderPane[SlsGamingProductOrder, SlsParticipationPools, SlsBet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "sls"
      )
    },
    classOf[UklsGamingProductOrder] -> { () =>
      new GenericOrderPane[UklsGamingProductOrder, UklsParticipationPools, UklsBet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "ukls"
      )
    },
    classOf[UktblsGamingProductOrder] -> { () =>
      new GenericOrderPane[UktblsGamingProductOrder, UktblsParticipationPools, UktblsBet](
        Seq(
          BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers),
          BetNumbersSpec(name = "Thunderball", cssClass = "extranumbers", x => Seq(x.thunderball))
        ), productCssClass = "uktbls"
      )
    },
    classOf[UspblsGamingProductOrder] -> { () =>
      new GenericOrderPane[UspblsGamingProductOrder, UspblsParticipationPools, UspblsBet](
        Seq(
          BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers),
          BetNumbersSpec(name = "Powerball", cssClass = "extranumbers", x => Seq(x.powerBall))
        ), productCssClass = "uspbls"
      )
    },
    classOf[XmaslGamingProductOrder] -> { () =>
      new GenericOrderPane[XmaslGamingProductOrder, XmaslParticipationPools, XmaslBet](
        Seq(BetNumbersSpec(name = "Numbers", cssClass = "numbers", _.numbers)), productCssClass = "xmasl"
      )
    }
  )
  
}