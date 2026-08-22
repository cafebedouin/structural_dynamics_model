% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV-Only Exclusive Inspiration Doctrine
 *   domain: religious/textual_criticism/theology
 *
 * SUMMARY:
 *   The KJV-Only exclusive inspiration reading asserts that the 1611 King
 *   James Version (and its subsequent minor revisions) is the sole inspired,
 *   inerrant English Bible — a providentially preserved text that supersedes
 *   the original Greek and Hebrew manuscripts. All other English translations
 *   are declared corrupted products of a polluted textual stream (the
 *   Alexandrian/Westcott-Hort line). This reading emerged from the
 *   19th-century controversy over the Revised Version and Westcott-Hort Greek
 *   text, hardening through the fundamentalist/modernist split into a
 *   distinct movement with its own institutions, publishing network, and
 *   seminaries. The constraint operates by gatekeeping textual legitimacy: to
 *   use a modern translation is to risk using a corrupt Bible, which
 *   threatens salvation assurance. Leadership enforces this through
 *   institutional control (ordination, affiliation, curriculum), publishing
 *   monopoly, and pastoral pressure. Modern translations and their users are
 *   the primary victims — suppressed as illegitimate. The KJV-Only leadership
 *   and its publishing ecosystem are the beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.78).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.85).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, snare).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV-Only Exclusive Inspiration Doctrine").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/textual_criticism/theology").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, '52bc0c09-3f0c-48e8-bb60-c43efc3ac533').
narrative_ontology:cs_kernel_codification('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', fixed_text).
narrative_ontology:cs_authority_grounding('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', extraction).
narrative_ontology:cs_interpretation_layer_present('52bc0c09-3f0c-48e8-bb60-c43efc3ac533').
narrative_ontology:cs_reading_relation('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', kjv_text_1611__functional_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', foundational, kjv_exclusive_inspiration).
narrative_ontology:cs_axiom_status(kjv_exclusive_inspiration, holdable).
narrative_ontology:cs_axiom_grounding('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', kjv_exclusive_inspiration, theological).
narrative_ontology:cs_axiom('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', foundational, modern_translations_corrupted).
narrative_ontology:cs_axiom_status(modern_translations_corrupted, holdable).
narrative_ontology:cs_axiom_grounding('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', modern_translations_corrupted, theological).
narrative_ontology:cs_axiom('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', secondary, textus_receptus_superiority).
narrative_ontology:cs_axiom_status(textus_receptus_superiority, holdable).
narrative_ontology:cs_axiom_grounding('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', textus_receptus_superiority, empirically_contingent).
narrative_ontology:cs_reference_frame('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', providential_preservation_1611).
narrative_ontology:cs_drift_state('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', contemporary_manuscript_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('52bc0c09-3f0c-48e8-bb60-c43efc3ac533', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishing_network).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_users).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, textual_criticism_scholars).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, non_kjv_only_congregations).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, bible_translation_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, lay_kjv_only_adherents).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, lay_kjv_only_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pastors, authors, and organizational heads who define and enforce the exclusive inspiration doctrine. They control institutional affiliation, ordination, and publishing access within the KJV-Only movement. Their authority derives from claiming unique access to God's preserved Word. Exit means abandoning the identity that constitutes their vocation and community standing.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Publishers, bookstores, and curriculum providers whose commercial viability depends on the KJV-Only market. They produce study materials, commentaries, and educational resources that assume KJV exclusivity. Their revenue stream would collapse if the constituency accepted modern translations as legitimate alternatives.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishing_network, beneficiary,
    organized, biographical, constrained, global).

% Christians who use modern translations (NIV, ESV, NASB, etc.) and are told their Bible is corrupted, inferior, or potentially damning. They bear the spiritual anxiety and social marginalization within KJV-Only spaces. Exit from the constraint means either adopting KJV-Only doctrine or leaving their faith community.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_users, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, modern_translation_users, excluded).

% Academic scholars who work with manuscript evidence, textual variants, and translation theory. Their expertise is dismissed as corrupting God's Word. They face professional marginalization in KJV-Only institutions but retain mainstream academic standing. Their exit option is strong — they operate in a different institutional ecosystem.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, textual_criticism_scholars, payer,
    organized, biographical, mobile, global).

% Churches and denominations that use modern translations and are targeted by KJV-Only evangelism, church-splitting campaigns, and accusations of apostasy. They bear the cost of defensive pastoral work and member attrition. Exit is constrained by geography and community ties.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, non_kjv_only_congregations, payer,
    moderate, generational, constrained, regional).

% Organizations like Wycliffe, SIL, and national Bible societies whose translation work is declared illegitimate. They lose funding, recruits, and access in KJV-Only zones. They have strong exit — they operate globally and the constraint only affects a subset of their domain.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, bible_translation_organizations, payer,
    institutional, generational, arbitrage, global).

% Ordinary believers who have been taught that salvation assurance depends on using the KJV. They receive certainty and community but pay with intellectual isolation, fear of alternative readings, and inability to engage broader Christian scholarship. Exit threatens their entire spiritual framework and social world.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, lay_kjv_only_adherents, beneficiary,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, lay_kjv_only_adherents, payer).

% Scholars of religion, theology, and textual criticism who study the KJV-Only movement as a sociological and doctrinal phenomenon. They analyze the constraint's structure without being subject to its enforcement.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, observing_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides absolute textual certainty for a community that equates doctrinal security with a single fixed English text. Solves the anxiety of textual variance by declaring one translation divinely preserved and all others corrupt.
% TRANSFER_FUNCTION: Moves epistemic authority, financial resources, and spiritual assurance from the broader Christian scholarly and translational ecosystem into the KJV-Only institutional network. Leadership controls what counts as Scripture; publishing network captures the market; adherents surrender interpretive autonomy for certainty.
% ABSENT_VOICES: The vast majority of global Christianity (Orthodox, Catholic, mainline Protestant, evangelical non-KJV-Only) that uses and produces modern translations. Historical KJV translators themselves, who explicitly disclaimed exclusive inspiration for their work. Manuscript evidence and linguistic data that contradict the perfect-preservation claim.
% DISAPPEARANCE_RATIONALE: If the exclusive inspiration claim vanished overnight, the KJV-Only institutional structure would lose its legitimating core. Publishing networks would face market competition from modern translations. Adherents would face a crisis of certainty requiring pastoral reconstruction. The global Bible translation enterprise would recover access to KJV-Only zones. The constraint actively organizes a segregated ecclesiastical and commercial ecosystem.
% FOUNDING_PROBLEM: Late 19th/early 20th century reaction to the Revised Version (1881) and Westcott-Hort Greek text: fear that textual criticism and modern translations were undermining biblical authority and doctrinal orthodoxy. The founding problem was perceived textual instability threatening Protestant confidence in Scripture.
% FOUNDING_PROBLEM_CORROBORATION: KJV-Only leadership attests the problem is live (modern translations continue to proliferate, textual criticism continues). Mainline scholars and major translation organizations (corroborating from outside the beneficiary set) attest the founding problem was based on a misunderstanding of textual criticism — the RV and subsequent translations improved accuracy, they did not corrupt the text. The manuscript evidence discovered since 1881 (papyri, Dead Sea Scrolls) corroborates that the textual stream has been clarified, not corrupted.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint captures the entire textual authority function for its constituency — it monopolizes what counts as God's Word, redirecting trust, money, and interpretive control to the KJV-Only network. Suppression is very high (0.85) because the constraint's persistence depends on actively delegitimizing alternatives: modern translations are not merely discouraged but declared spiritually dangerous; scholars are marginalized; congregations are targeted for conversion or splitting. Theater ratio is moderate (0.42): the textual scholarship apparatus (Greek/Hebrew study, manuscript evidence) is maintained performatively but only to confirm the predetermined conclusion; the real function is boundary maintenance. Accessibility collapse is high (0.72) because once the exclusive inspiration premise is accepted, alternatives are not just wrong — they are epistemically inaccessible (a corrupt Bible cannot correct the true Bible). Resistance is significant (0.68) from the global scholarly and translational establishment, but the constraint's identity-locked adherents are insulated from it.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat, the constraint is experienced as faithful stewardship of God's preserved Word — genuine coordination of doctrinal purity. From the lay adherent seat, it is experienced as security — the only anchor in a shifting world. From the modern translation user seat, it is experienced as spiritual coercion — being told their Bible is damning. From the scholar seat, it is experienced as epistemic closure — evidence excluded by doctrinal fiat. The engine computes these divergences from the structural data; the claimed_type (snare) reflects the analytical seat's assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   KJV-Only leadership are full beneficiaries (d ≈ 0.05) — they control the epistemic gate, collect institutional rents, and their identity is fused with the constraint. Publishing network are beneficiaries with constrained exit (d ≈ 0.2) — they profit but could pivot if the market shifted. Lay adherents are identity-locked (d ≈ 0.85) — they receive certainty but pay with total epistemic dependence; exit means spiritual crisis. Modern translation users are payers with constrained exit (d ≈ 0.75) — they bear spiritual anxiety and social cost but can leave the KJV-Only sphere. Textual scholars are payers with mobile exit (d ≈ 0.4) — dismissed but institutionally independent. Translation organizations are payers with arbitrage exit (d ≈ 0.2) — affected only at the margin. Observing theologians are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (textual instability threatening biblical authority) has been substantially addressed by modern textual criticism — we have more and earlier manuscripts than ever, and translation theory has advanced. But the arrangement persists and has intensified because it now serves as the identity boundary for a distinct subculture. The mandate (defend biblical authority) has atrophied into a marker of group membership. The constraint is not coordination masquerading as extraction — it is extraction that has *become* the coordination function for the community. The coordination is real (it unites the group) but the object of coordination (exclusive inspiration) is false by the standards the movement itself claims (textual fidelity). This is a snare, not a tangled rope, because the coordination function is inseparable from the extraction: the group exists *because* it excludes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to the kjv_text_1611 kernel differ from its siblings, and where is the disagreement located?',
    'Compare the victim sets, beneficiary structures, and authority claims across all three readings. The exclusive_inspiration_reading uniquely makes modern translations and their users into victims; the other readings do not.',
    'If the kernel is the 1611 text itself, this reading extracts from the kernel''s reception history by freezing it. The other readings allow the kernel to function as a living textual tradition. This reading''s high extractiveness is structurally tied to its committer-frame position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'This constraint is one reading of the contested kjv_text_1611 kernel. The exclusive_inspiration_reading declares the KJV text itself as the inspired standard, foreclosing revisable_translation_reading and creating victims where other readings see beneficiaries. The disagreement is located in the authority_grounding: this reading claims providential preservation (extraction grounding), while siblings claim scholarly improvability (expertise grounding) or functional complementarity (practice grounding).').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional enforcement, publishing monopoly) or internalized (adherents'' fused identity, spiritual anxiety)?',
    'Post-exit suppression trajectory study: track former KJV-Only adherents — does the sense of textual corruption and spiritual danger persist after leaving the institutional structure?',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression internally after institutional exit. This would increase the constraint''s snare character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in an identity-locked interpersonal/institutional constraint.').

omega_variable(
    providential_preservation_falsifiability,
    'Is the providential preservation claim empirically falsifiable, and does its unfalsifiability serve as an extraction mechanism?',
    'Manuscript evidence (papyri, majuscules, Dead Sea Scrolls) consistently contradicts the Textus Receptus superiority claim. The reading''s response pattern to counter-evidence (dismissal, conspiracy theories about manuscript corruption) indicates whether the claim functions as a belief or as a boundary marker.',
    'If the claim operates as an unfalsifiable boundary marker rather than a testable historical claim, the constraint''s coordination function is identity maintenance, not textual fidelity — confirming snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(providential_preservation_falsifiability, conceptual, 'Whether the core doctrinal claim is a genuine empirical hypothesis or an identity-constituting axiom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 1881, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1881, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1881, 0.15).
narrative_ontology:measurement(kjv__tr_t1930, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1930, 0.22).
narrative_ontology:measurement(kjv__tr_t1960, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(kjv__tr_t1985, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(kjv__tr_t2000, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(kjv__tr_t2024, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1881, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1881, 0.35).
narrative_ontology:measurement(kjv__be_t1930, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1930, 0.52).
narrative_ontology:measurement(kjv__be_t1960, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1960, 0.65).
narrative_ontology:measurement(kjv__be_t1985, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 1985, 0.72).
narrative_ontology:measurement(kjv__be_t2000, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(kjv__be_t2024, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t1881, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1881, 0.45).
narrative_ontology:measurement(kjv__su_t1930, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1930, 0.62).
narrative_ontology:measurement(kjv__su_t1960, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1960, 0.73).
narrative_ontology:measurement(kjv__su_t1985, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(kjv__su_t2000, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(kjv__su_t2024, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__exclusive_inspiration_reading, 0.08).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, bible_translation_funding_ecosystem).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, evangelical_seminary_accreditation).

% DUAL FORMULATION NOTE:
% Part of the kjv_text_1611 constraint family. This reading (exclusive_inspiration) has high ε (0.78) because it freezes the kernel and extracts from the broader translational ecosystem. The revisable_translation_reading has low ε (~0.15) — it treats the KJV as a revisable historical artifact. The functional_equivalence_reading has moderate ε (~0.35) — it coordinates multiple translations without suppressing alternatives. The ε-invariance principle requires separate stories because the referent (the KJV text and its authority) is evaluated by different lights in each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__exclusive_inspiration_reading, powerless, 0.85).
constraint_indexing:directionality_override(kjv_text_1611__exclusive_inspiration_reading, moderate, 0.75).
constraint_indexing:directionality_override(kjv_text_1611__exclusive_inspiration_reading, organized, 0.4).
constraint_indexing:directionality_override(kjv_text_1611__exclusive_inspiration_reading, institutional, 0.05).
constraint_indexing:directionality_override(kjv_text_1611__exclusive_inspiration_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
