package domain

import monix.execution.{Scheduler => MonixScheduler}

trait PoolValidatorFactory {
  def getValidator(resourceProvider: PoolResourceProvider, credentialsManager: CredentialsManager, scheduler: MonixScheduler): PoolValidator
}

object PoolValidatorFactoryImpl extends PoolValidatorFactory {
  def getValidator(resourceProvider: PoolResourceProvider, credentialsManager: CredentialsManager, scheduler: MonixScheduler): PoolValidator = {
    new PoolValidatorImpl(resourceProvider, credentialsManager, DefaultSignatureAlgMapper)(scheduler)
  }
}
