import clsx from 'clsx';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'Annotation and Learning',
    description: (
      <>
        Paw provides a complete solution for making annotations and language learning
        tools, right inside Emacs. Add highlights, bookmarks, and notes with ease.
      </>
    ),
  },
  {
    title: 'Multi-language Support',
    description: (
      <>
        Support for multiple languages with dictionary integration, translation,
        and pronunciation features. Perfect for language learners.
      </>
    ),
  },
  {
    title: 'Cross-platform',
    description: (
      <>
        Works on PC and mobile devices. Synchronize your learning progress
        across all your devices using SQLite database.
      </>
    ),
  },
];

function Feature({title, description}) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}