% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment — Performance-Only Reading (Study as Archival Preservation, Not Occupation)
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This story instantiates one reading — performance_only — of the contested
 *   temple_sacrifice_commitment kernel. The natural-language label 'the
 *   sacrifice commandment after the Temple's destruction' covers at least
 *   four structurally distinct claims about what, if anything, currently
 *   occupies the commitment. This reading holds that the commandment requires
 *   material instantiation (actual animal sacrifice on a functioning altar
 *   under priestly service) and that study, however rigorous, is archival
 *   preservation of a defunct practice rather than occupation of the
 *   commandment itself. This is a low-extraction, low-suppression rope: it
 *   coordinates a real epistemic-preservation function (keeping procedural
 *   knowledge alive) without significant coercion, because no one is forced
 *   to accept the reading and no material sacrifice is actually being
 *   performed under its authority. The theater_ratio is comparatively high
 *   (0.55-0.68) because a large share of the activity this reading validates
 *   — genealogical registries, implement replicas, ceremonial rehearsals — is
 *   performative preparation for a contingency that has not materialized in
 *   nearly two millennia, and that share has grown, not shrunk, as
 *   restoration-oriented institutionalization has professionalized. There is
 *   no current victim set: the commitment is a dormant husk, and the
 *   reading's cost (denying ordinary practitioners a sense of doctrinal
 *   completeness through study) is diffuse and non-coercive. The omega
 *   variables capture the genuinely open question this reading raises but
 *   does not itself resolve: what happens to the currently-absent interest
 *   set (future priestly claimants, ecological/animal-welfare concerns) if
 *   restoration is someday actually attempted without ethical re-examination
 *   of the sacrificial economy in modern conditions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.12).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.08).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment — Performance-Only Reading (Study as Archival Preservation, Not Occupation)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '8055a831-f72d-4f02-b6c4-25f4bad650c3').
narrative_ontology:cs_kernel_codification('8055a831-f72d-4f02-b6c4-25f4bad650c3', fixed_text).
narrative_ontology:cs_authority_grounding('8055a831-f72d-4f02-b6c4-25f4bad650c3', lineage).
narrative_ontology:cs_interpretation_layer_present('8055a831-f72d-4f02-b6c4-25f4bad650c3').
narrative_ontology:cs_reading_relation('8055a831-f72d-4f02-b6c4-25f4bad650c3', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('8055a831-f72d-4f02-b6c4-25f4bad650c3', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_reading_relation('8055a831-f72d-4f02-b6c4-25f4bad650c3', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('8055a831-f72d-4f02-b6c4-25f4bad650c3', foundational, material_instantiation_is_constitutive_of_occupation).
narrative_ontology:cs_axiom_status(material_instantiation_is_constitutive_of_occupation, holdable).
narrative_ontology:cs_axiom_grounding('8055a831-f72d-4f02-b6c4-25f4bad650c3', material_instantiation_is_constitutive_of_occupation, deontological).
narrative_ontology:cs_axiom('8055a831-f72d-4f02-b6c4-25f4bad650c3', foundational, study_without_performance_is_preservation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_without_performance_is_preservation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('8055a831-f72d-4f02-b6c4-25f4bad650c3', study_without_performance_is_preservation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('8055a831-f72d-4f02-b6c4-25f4bad650c3', temple_era_priestly_service_standard).
narrative_ontology:cs_drift_state('8055a831-f72d-4f02-b6c4-25f4bad650c3', post_destruction_diaspora_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8055a831-f72d-4f02-b6c4-25f4bad650c3', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, halakhic_study_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, restoration_oriented_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__performance_only, diaspora_observant_communities).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, material_instantiation_requirement).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, sacrifice_law_as_praxis_not_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshivot and study circles that teach the sacrificial order (Seder Kodashim) as a discipline of legal reasoning rather than devotional occupation of the commandment. On the performance-only reading, this study is honored as scholarship and pedagogy — it preserves technical knowledge that would be needed if performance ever resumed — but is not credited as fulfilling the commandment itself. The institutions gain prestige and continuity of a curriculum without needing to defend a claim that study itself discharges the obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, halakhic_study_institutions, beneficiary,
    organized, generational, mobile, national).

% Groups preparing ritual implements, priestly genealogies, and procedural knowledge toward a future rebuilt Temple. The performance-only reading validates their entire project: if study alone occupied the commitment, their preparatory work toward material rebuilding would be redundant. Because performance is held to be the only true instantiation, their activity is structurally necessary rather than optional piety.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, restoration_oriented_movements, beneficiary,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__performance_only, restoration_oriented_movements, agenda_setter).

% Ordinary observant Jews who study the sacrificial texts liturgically (e.g., in daily prayer recitations referencing korbanot) but have no access to material performance and no expectation of it in their lifetimes. Under this reading, their devotional practice is denied full doctrinal weight as occupation of the commandment — it is categorized as commemorative or preparatory at best, archival at worst. This costs them a sense of religious completion without imposing any material burden; the cost is doctrinal/psychological, not economic or coercive.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, diaspora_observant_communities, payer,
    moderate, biographical, constrained, global).

% Individuals and communities whose claimed kohanic or Levitical descent would matter enormously if performance resumed but currently confers no practical status. They are not consulted in the doctrinal debate over whether study occupies the commitment, yet their genealogical standing is the silent precondition the performance-only reading depends on and reactivates as consequential.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, future_priestly_lineage_claimants, excluded,
    powerless, civilizational, trapped, national).

% Not an agent in the classical sense, but named for completeness: the non-agent interest set (animal welfare, ecological/logistical concerns of a functioning sacrificial economy) is entirely absent from the doctrinal conversation about whether study or performance occupies the commitment. If restoration is ever attempted under this reading without ethical re-examination, this is the interest set most directly implicated and least represented.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, prospective_animals_and_ecological_stakeholders, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__performance_only, prospective_animals_and_ecological_stakeholders).

% Academic and rabbinic-academic observers who trace how the performance-only reading interacts with the other three readings of the same kernel (study_as_exercise, hybrid_preparatory, symbolic_transformation) across different communities and historical periods. They do not adjudicate which reading is correct but document how each reading structures different institutional and ethical consequences.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, comparative_halakhic_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves precise technical and legal knowledge of sacrificial procedure across a period of non-performance, so that if material conditions for restoration ever arise, the community possesses continuous, unbroken procedural competence rather than having to reconstruct practice from fragmentary sources.
% TRANSFER_FUNCTION: Moves prestige and doctrinal authority toward institutions and movements whose activity (technical study, restoration preparation) is validated as necessary precisely because it is NOT sufficient — the commandment remains unoccupied until performance resumes. It moves a sense of doctrinal completeness away from ordinary practitioners whose devotional study is recategorized as commemorative rather than commandment-fulfilling.
% ABSENT_VOICES: Future priestly lineage claimants and the non-agent interest set (animals, ecological/logistical stakeholders of an actual sacrificial economy) have no voice in the current doctrinal debate, yet the performance-only reading is precisely the reading that would make their standing and interests suddenly consequential if acted upon. Ordinary diaspora communities whose devotional practice is downgraded to non-occupation are also not the ones adjudicating the reading — the adjudication is conducted by legal authorities and restoration-oriented institutions.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished (i.e., were universally abandoned in favor of a sibling reading), restoration-oriented movements would lose their doctrinal warrant for treating physical preparation as necessary, and study institutions might gain doctrinal credit for occupying the commandment through study alone — a real reallocation of religious authority and institutional purpose. Ordinary practitioners' devotional experience might also shift in felt completeness. Whether this counts as the world 'rearranging' or 'staying the same' is itself contested between the reading's proponents (who see performance-only as doctrinally load-bearing) and adherents of sibling readings (who see the distinction as a technical dispute with limited practical consequence while the Temple remains unbuilt).
% FOUNDING_PROBLEM: The commandment to offer sacrifices presupposes a Temple and priestly service; the Temple's destruction created an acute doctrinal problem — how to relate to a commandment whose material preconditions no longer exist. The performance-only reading was built to preserve the commandment's full weight and specificity (rather than diluting it into something achievable by study) while honestly acknowledging that the commandment is not currently being fulfilled.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities within the performance-only tradition (e.g., strands of Maimonidean and later halakhic reasoning that treat mitzvot tluyot ba'aretz/batemple as suspended rather than transformed) attest the founding problem persists exactly as originally framed — the commandment awaits its material conditions. Comparative religious-studies scholars, writing from outside any of the four reading-traditions, corroborate that this reading has structurally shaped which communities invest in physical restoration preparation (priestly genealogy registries, ritual implement reconstruction) versus which redirect resources toward study or liturgical substitution — an effect independently observable in institutional funding and activity patterns, not merely asserted by the reading's proponents.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) and rising only slightly because the reading imposes no material transfer — no one pays a toll to hold this doctrinal position, and no material sacrifice is presently occurring for anyone to be exploited by. Suppression is authored low (0.08) because rival readings (study_as_exercise, hybrid_preparatory, symbolic_transformation) are openly held by other communities without persecution; this is intra-tradition doctrinal pluralism, not enforced orthodoxy. Theater ratio is the most diagnostically interesting metric here: it is authored moderately high and rising because performance-only's practical output — genealogical registries, replica implements, procedural rehearsals absent an altar — is precisely the kind of activity that looks like performance of a function that cannot actually be performed. Accessibility collapse and resistance are both moderate (0.40, 0.35): sibling readings remain fully available and are actively chosen by other communities, so alternatives have not collapsed, but resistance exists in the sense that communities holding sibling readings actively contest this reading's exclusivist claim about what counts as occupation.
 *
 * DIRECTIONALITY LOGIC:
 *   Restoration-oriented movements and study institutions are the structural beneficiaries: their activities gain doctrinal necessity and institutional legitimacy precisely because this reading denies that study alone suffices. Diaspora observant communities bear a diffuse, non-material cost — a doctrinal downgrade of their devotional practice's completeness — positioning them as mild payers with no coercive extraction involved. Future priestly lineage claimants and the non-agent ecological/animal-welfare interest set are excluded from the conversation entirely; they are currently inert but are the ones whose position would be reactivated with real stakes if this reading's implied restoration project were ever pursued.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this doctrinal position as pure extraction: there is no beneficiary skimming rents off a captive population, and the coordination function (preserving technical knowledge against a contingency) is genuine, however uncertain its future payoff. Equally, the classification resists treating the position as simple, cost-free coordination: the theater_ratio trend and the excluded future-victim set show that 'preparation for restoration' carries a latent extraction risk that has not yet materialized but is structurally primed to if restoration were attempted without addressing the excluded interests (lineage claimants, animal welfare, modern ethical review) that the current dormant-husk status conveniently defers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the performance_only reading''s claim that study is ''archival preservation, not occupation'' logically foreclose the study_as_exercise reading''s claim that study IS performance of the divine command, or can both be held as coexisting positions across different communities without internal contradiction in either community''s framework?',
    'Analysis of whether any single halakhic authority or community has attempted to hold both positions simultaneously without contradiction; survey of contemporary poskim''s treatment of the two claims as mutually exclusive versus complementary.',
    'If genuinely foreclosing, adopting performance_only within a community structurally forecloses treating study as commandment-fulfilling within that same community''s framework, which would elevate the practical stakes of doctrinal choice for study institutions. If merely coexisting across communities, the reading is a live minority-vs-majority position rather than a logically exclusive claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether performance_only logically forecloses study_as_exercise or merely coexists with it across communities.').

omega_variable(
    restoration_ethics_deferral,
    'Because the commitment is currently a dormant husk under this reading, no current victim set exists — but does deferring ethical re-examination of sacrificial practice (animal welfare, priestly caste exclusivity, ecological impact of a restored Temple economy) to a hypothetical future restoration constitute a hidden extraction risk that the reading''s current low-epsilon profile masks?',
    'Track whether restoration-oriented movements actively engage in ethical reformulation work (e.g., modern halakhic discussion of animal welfare in sacrificial law) versus treating ethical questions as deferred until restoration is imminent.',
    'If ethical re-examination is genuinely deferred rather than ongoing, the reading''s current low extraction score is a temporal artifact — the constraint may transition to substantially higher extraction and a real victim set (excluded lineage claimants elevated to gatekept status, animal welfare costs) the moment material restoration is attempted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_ethics_deferral, empirical, 'Whether the current absence of victims is genuine or merely deferred until hypothetical restoration.').

omega_variable(
    natural_kernel_vs_constructed_doctrine,
    'Is the requirement that sacrifice law demands material instantiation an irreducible feature of the original commandment''s own textual and legal structure (a natural reading of the source material), or is it a constructed doctrinal choice that happens to benefit restoration-oriented institutions by validating their preparatory activity as necessary?',
    'Comparative textual analysis of the earliest strata of legal sources (Mishnah, Tosefta, early amoraic discussion) versus later systematizations, to assess whether the material-instantiation requirement is present from the earliest layers or is a later doctrinal hardening coincident with the emergence of organized restoration movements.',
    'If the requirement is a genuinely early and textually irreducible feature, the reading''s beneficiary structure (restoration movements, study institutions) is incidental to an independently-grounded doctrinal fact. If it is a later hardening correlated with institutional interest, the reading looks more like a constructed doctrine that happens to serve identifiable beneficiaries — relevant because this reading''s claimed_type is rope, not mountain, but the underlying textual-naturalness question still bears on how much independent weight the doctrine should carry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_kernel_vs_constructed_doctrine, conceptual, 'Whether the material-instantiation requirement is textually primary or a later doctrinal construction coincident with institutional interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.55).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__performance_only, theater_ratio, 20, 0.58).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__performance_only, theater_ratio, 40, 0.6).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__performance_only, theater_ratio, 60, 0.63).
narrative_ontology:measurement(temp_tr_t80, temple_sacrifice_commitment__performance_only, theater_ratio, 80, 0.66).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__performance_only, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__performance_only, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__performance_only, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__performance_only, base_extractiveness, 60, 0.1).
narrative_ontology:measurement(temp_be_t80, temple_sacrifice_commitment__performance_only, base_extractiveness, 80, 0.11).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__performance_only, base_extractiveness, 100, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__performance_only, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This story is one of four constraint stories decomposing the natural-language label 'the sacrifice commandment's status after the Temple's destruction' into structurally distinct claims, per the ε-invariance principle. performance_only claims the commandment remains unoccupied absent material sacrifice (low epsilon, rope, dormant-husk coordination function). study_as_exercise claims study itself occupies the commandment (different epsilon profile — likely lower suppression, different beneficiary set centered on study institutions alone). hybrid_preparatory claims study maintains the commitment in a suspended intermediate state. symbolic_transformation claims an authorized substitution has occurred (prayer/study as the NEW instantiation, not a stand-in for a suspended one) — this is the most structurally different sibling, since it denies the commandment is dormant at all. All four are linked bidirectionally via affects_constraints because a change in institutional or doctrinal weight given to any one reading structurally pressures resource allocation and legitimacy claims for the others (e.g., growth of restoration-oriented institutions under performance_only draws attention and resources away from study-centered legitimation under study_as_exercise).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
