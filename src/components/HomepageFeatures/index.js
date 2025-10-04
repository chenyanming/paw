import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'Language Learning',
    description: (
      <>
        Paw makes language learning in Emacs effortless. Click on any word to get definitions, translations, and add them to your study lists.
      </>
    ),
  },
  {
    title: 'Reading & Annotation',
    description: (
      <>
        Read books, articles, and documents while making annotations. Highlights, bookmarks, and notes are all saved in a sqlite database.
      </>
    ),
  },
  {
    title: 'Emacs Integration',
    description: (
      <>
        Works seamlessly with nov-mode, org-mode, eww, pdf-tools, and many other Emacs modes. Study for a life in Emacs.
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
