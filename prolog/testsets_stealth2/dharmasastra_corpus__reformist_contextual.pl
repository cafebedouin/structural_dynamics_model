% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Reformist-Contextual Settlement of the Dharmasastra Corpus
 *   domain: religious law/textual interpretation/normative authority
 *
 * SUMMARY:
 *   The reformist-contextual settlement is the standing arrangement by which
 *   the Dharmasastra corpus retains scriptural authority while its birth-rank
 *   social prescriptions are reclassified as historically conditioned
 *   accretion or restated as stages of spiritual progress rather than civil
 *   rank. It emerged from the nineteenth-century legitimacy crisis (Brahmo
 *   and Arya Samaj critique, missionary attack, colonial ethnography) and
 *   matured through Vivekananda's universalist framing, Gandhian contextual
 *   reading, and the post-constitutional accommodation between personal-law
 *   autonomy and equality guarantees. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as tangled_rope from the authoring seat — the
 *   settlement genuinely coordinates continuity and identity while
 *   asymmetrically retaining rank's costs — and the metrics are authored
 *   descriptively at medium intensity; the engine computes per-seat
 *   classifications from the structural data, and this file does not
 *   reconcile claim to metrics. Kernel decomposition: the colloquial label
 *   'Dharmasastra' covers three structurally distinct claims instantiated as
 *   three linked stories (orthodox_literalist, reformist_contextual — this
 *   file — and abolitionist_rejection), each with its own epsilon over the
 *   same referent, the standing corpus-based order.
 *
 * KEY AGENTS:
 *   - - reformist_acharya_lineages: Agenda-setting interpreter class (institutional/identity_locked) — administers meaning, collects prestige and endowments
 *   - - upper_caste_lay_devotees: Primary beneficiary constituency (organized/constrained) — status and continuity preserved under softened frame
 *   - - dalit_communities: Principal residual target (organized/trapped) — bears the symbolic and material residue of rank
 *   - - shudra_caste_communities: Dual-positioned middle (moderate/constrained) — pays deference, gains limited mobility
 *   - - orthodox_pandit_establishment: Excluded rival authority (institutional/identity_locked) — rejects the settlement from outside its conversations
 *   - - abolitionist_ambedkarite_movements: Excluded radical critic (organized/mobile) — attests the settlement is laundering, not solving
 *   - - diaspora_hindu_communities: Secondary beneficiary (moderate/mobile) — consumes the softened frame at distance from enforcement sites
 *   - - state_judiciary_and_legislature: Analytical observer (institutional/analytical) — sets outer limits, rarely touches doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.45).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.35).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.45).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Reformist-Contextual Settlement of the Dharmasastra Corpus").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious law/textual interpretation/normative authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'd561a027-eb73-4d65-9d7d-23ee69d83e65').
narrative_ontology:cs_kernel_codification('d561a027-eb73-4d65-9d7d-23ee69d83e65', fixed_text).
narrative_ontology:cs_authority_grounding('d561a027-eb73-4d65-9d7d-23ee69d83e65', lineage).
narrative_ontology:cs_interpretation_layer_present('d561a027-eb73-4d65-9d7d-23ee69d83e65').
narrative_ontology:cs_reading_relation('d561a027-eb73-4d65-9d7d-23ee69d83e65', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('d561a027-eb73-4d65-9d7d-23ee69d83e65', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('d561a027-eb73-4d65-9d7d-23ee69d83e65', foundational, dharma_ethical_core_transhistorical).
narrative_ontology:cs_axiom_status(dharma_ethical_core_transhistorical, holdable).
narrative_ontology:cs_axiom_grounding('d561a027-eb73-4d65-9d7d-23ee69d83e65', dharma_ethical_core_transhistorical, deontological).
narrative_ontology:cs_axiom('d561a027-eb73-4d65-9d7d-23ee69d83e65', foundational, rank_prescriptions_historically_contingent).
narrative_ontology:cs_axiom_status(rank_prescriptions_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('d561a027-eb73-4d65-9d7d-23ee69d83e65', rank_prescriptions_historically_contingent, empirically_contingent).
narrative_ontology:cs_reference_frame('d561a027-eb73-4d65-9d7d-23ee69d83e65', separable_ethical_core_canon).
narrative_ontology:cs_drift_state('d561a027-eb73-4d65-9d7d-23ee69d83e65', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d561a027-eb73-4d65-9d7d-23ee69d83e65', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_acharya_lineages).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, upper_caste_lay_devotees).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, diaspora_hindu_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, shudra_caste_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, shudra_caste_communities).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, hermeneutic_separability_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, constitutional_equality_compatibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the mathas, missions, publishing houses, and teacher-training circuits that decide what the corpus means for contemporary life. They publish commentaries recasting birth-ranked social orders as spiritual stages or historical accretion, ordain teachers, and set curricula for schools and diaspora congregations. Prestige, endowment income, and the authority to speak for the tradition flow to them; their position depends on the corpus remaining authoritative, so they permanently manage a boundary dispute with literalist pandits on one side and renunciationist critics on the other. Leaving the interpretive role would mean surrendering the institution they embody.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_acharya_lineages, agenda_setter,
    institutional, generational, identity_locked, global).

% Keep community standing, marriage networks, festival leadership, and ritual precedence under a settlement that formally disavows rank while leaving household and kinship practice largely untouched. They fund the institutions, staff the committees, and supply the volunteers; the settlement lets them honor the texts and their grandparents' ways at once. Opting out would mean breaking with extended family and community.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, upper_caste_lay_devotees, beneficiary,
    organized, biographical, constrained, national).

% Live with the settlement's residue: ritual rankings restated as stages of spiritual progress still place them last, hereditary temple and priesthood posts stay closed, village marriage arrangements still screen by birth, and manual scavenging persists in pockets. Formal equality arrived from courts and statutes outside the settlement; inside it, recognition arrives as symbolism — statues, festival invitations, speeches — while control of sacred infrastructure rarely moves. Organized movements and conversion offer ways out, but conversion costs reserved protections, kinship, and physical safety, so most remain inside a frame that honors them rhetorically and excludes them operationally.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, dalit_communities, payer,
    organized, generational, trapped, national).

% Occupy the middle of the settlement: reformist framing opened government employment, land markets, and vernacular devotional movements that bypass Sanskritic gatekeeping, and many have risen economically. Yet the interpretive seats remain closed to them, and the spiritual-stages vocabulary still ranks their traditions below the classical canon. They pay deference and tithes into institutions they help sustain but do not steer.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, shudra_caste_communities, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, shudra_caste_communities, beneficiary).

% Hold that the corpus prescribes an eternal social order and read the reformist commentaries as betrayal dressed as fidelity. They retain ritual clients and traditional seminaries but sit outside the conferences, curricula, and media circuits where the settlement's meaning is made; their objections register as noise within the reformist frame rather than as a rival authority to be answered.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_pandit_establishment, excluded,
    institutional, generational, identity_locked, continental).

% Organized currents — Ambedkarite Buddhist sanghas, anti-caste literary circles, student federations — that hold the corpus irredeemable and the settlement a laundering device. They convert, publish, march, and litigate; they stand outside the settlement's conversations by choice and by exclusion, and their pressure at the boundary is what forces the settlement to keep demonstrating its reforms.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_ambedkarite_movements, excluded,
    organized, generational, mobile, national).

% Migrant and second-generation congregations abroad teach the tradition as philosophy, yoga, and ethics with the social-order chapters footnoted as history. The settlement gives them a presentable heritage in pluralist societies; their donations sustain the institutions that produce it. Distance from the villages where rank still bites makes the softened reading easy to hold.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, diaspora_hindu_communities, beneficiary,
    moderate, biographical, mobile, global).

% Courts and parliaments set the outer limits: temple-entry acts, anti-discrimination law, personal-law reform, and periodic litigation over ritual office. They take testimony from every seat above, occasionally redraw what the settlement may enforce, and otherwise leave doctrinal meaning to the interpreters.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, state_judiciary_and_legislature, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__reformist_contextual, reformist_acharya_lineages).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__reformist_contextual, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a several-hundred-million-member tradition continuous through a legitimacy crisis: one canon, one festival calendar, one teaching infrastructure, and a shared identity sustained across regions and generations while the corpus's social-order chapters are reclassified as history. Without a mediating reading, the community faced schism between literal observance and wholesale abandonment.
% TRANSFER_FUNCTION: Moves interpretive authority, endowment income, and public representation to the acharya-and-mission class; moves deference, attendance, and donations from lay constituencies; and leaves the residual costs of ranked practice — closed ritual offices, birth-screened marriage, scavenging labor — concentrated on dalit and, secondarily, shudra communities, while transferring the moral credit for having reformed to the upper strata.
% ABSENT_VOICES: Dalit voices are admitted selectively — amplified when they endorse the reformed frame, quieted when they press for transfer of ritual office or land; orthodox pandits are outside the conferences and curricula where meaning is made; abolitionist organizers are handled as adversaries of the community rather than parties to the argument. They are located in separate literary spheres, political parties, and sanghas beyond the settlement's institutions.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would force the binary the settlement exists to prevent: congregations would sort between literal observance and abandonment, diaspora institutions would lose their teaching frame, interfaith and state-facing representation would fragment, and the boundary organizations that channel dalit claims into negotiation would lose their counterpart. Village practice would not revert to literal Manusmriti enforcement, but the mediating machinery — schools, missions, commentaries, representation — would reorganize around whichever successor frame captured each institution.
% FOUNDING_PROBLEM: The collision between the corpus's inherited social prescriptions and the equality norms of the modern state and missionary critique: how to keep the texts authoritative and the community intact while shedding rules no longer defensible.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration of the founding crisis is strong from outside the beneficiary set: colonial administrative records, missionary polemics, and the constituent-assembly debates of the 1940s all attest the legitimacy emergency independently, and academic indology corroborates the historical-conditioning premise the settlement rests on. Corroboration of the settlement's adequacy, by contrast, comes almost entirely from inside the benefiting parties; ambedkarite scholarship — outside that set — attests the crisis was real while disputing that it admits this solution, and no neutral seat currently attests the founding problem as solved.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).
:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.45 (medium): strict enforcement of birth-rank prescription has ended and legal equality binds from outside, but the settlement's own machinery — spiritual-stages vocabulary, hereditary ritual offices, birth-screened marriage norms — continues to rank and exclude, with costs concentrated on dalit and shudra seats. Suppression is authored at 0.35 as a raw structural property (unscaled by power or scope; only extractiveness is scaled by directionality and scope in the engine): the settlement's hold now runs through kinship economics, community sanction, and internalized acceptance rather than coercive machinery. Theater_ratio is 0.45 and rising across the interval: as material reform succeeded, a growing share of settlement activity became performative inclusion — unity festivals, symbolic appointments, commemorative speeches — relative to transfers of actual ritual authority. Accessibility_collapse is 0.40: alternatives demonstrably survive (conversion movements, secular exit, orthodox parallel practice), so the settlement does not close the option space. Resistance is 0.65: ambedkarite assertion, Navayana conversion, anti-caste literature, and feminist critique constitute sustained organized pressure. The temporal series run on ONE shared grid (T=0..150, mapping approximately 1875-2025) with all three metrics authored at every point; suppression_requirement is tracked because the story specifically traces enforcement-capacity migration — from dense community-level sanction at T=0 to thin, internalized maintenance at T=150 — a falling trajectory modeling enforcement decay, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the acharya seat the settlement is faithful interpretation: the texts preserved, the community intact, cruelty shed. From the dalit seat the same settlement is managed subordination: inclusion rhetoric without transfer of ritual office, spiritual-stages language that re-ranks while denying it ranks. Upper-caste laity experience a genuine moral achievement; shudra communities experience a mixed ledger of mobility gained and authority withheld. The engine derives these divergent per-seat classifications from the structural data (roles, power, exit); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the acharya lineages (agenda-setter collecting prestige and endowments, identity-locked into the interpretive role), upper-caste laity (status preserved, exit costly through kinship rupture), and diaspora communities (softened frame consumed at distance, mobile exit). Victim declarations drive high directionality for dalit communities (trapped: conversion exits exist but carry reservation loss, family rupture, and physical risk) and shudra communities (constrained: mobility gained but interpretive seats closed). The dual-positioned shudra seat sits mid-scale — payer with a genuine secondary benefit — which the structural derivation should register as intermediate rather than terminal. Orthodox pandits and abolitionist movements are excluded seats: they shape the settlement's boundary conditions but collect nothing from and pay nothing into its operation, so they feed the consensus-provenance check rather than the extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this settlement as tangled_rope prevents two opposite mislabels. Reading it as pure rope would erase the dalit ledger — the closed priesthood, screened marriages, and re-ranking vocabulary are real costs paid through the same structure that coordinates continuity. Reading it as pure snare would erase the genuine coordination achievement — a several-hundred-million-member tradition held together through a legitimacy crisis without schism, with real material reforms delivered along the way. The R5 interview locates the mandatrophy question precisely: the founding problem (retaining textual authority under equality norms) is CONTESTED, not dead — reformists attest it is live and unsolved-but-solvable, abolitionists attest it was never solvable inside the frame. If the founding problem is in fact dead (equality fully absorbed, rank fully vestigial), the mismatch consumer flags a zombie settlement maintained theatrically; the rising theater_ratio series is the observable that would confirm or refute that flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the reformist_contextual reading of the dharmasastra_corpus kernel — what structural facts would the sibling readings (orthodox_literalist, abolitionist_rejection) assert that this file deliberately does not?',
    'Author and compile the two sibling stories; compare victim sets, epsilon, and coordination-function declarations across the three-file family.',
    'Orthodox_literalist restores the full birth-rank victim set and raises epsilon sharply; abolitionist_rejection denies the coordination function entirely and drives epsilon toward its maximum. Neither outcome belongs inside this file; each is a distinct constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is one reading of a three-reading kernel; sibling readings are separate files.').

omega_variable(
    separability_thesis_status,
    'Is dharma-as-righteous-conduct genuinely separable from the corpus''s rank prescriptions, or is hierarchy constitutive of the dharma concept such that the ''ethical core'' is a modern selection?',
    'Textual stratigraphy and reception history: establish whether varna discourse appears in the earliest strata or accumulates in later smriti layers, and whether any pre-modern commentator treated conduct rules as detachable from birth rules.',
    'If hierarchy is constitutive, the settlement''s coordination function is cover for retention of rank and the story trends toward pure extraction; if separable, the medium-extraction profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_thesis_status, conceptual, 'Whether the reading''s foundational separability premise is true of the corpus or imposed on it.').

omega_variable(
    symbolic_versus_material_reduction,
    'Has the settlement reduced the burden on subordinated communities in fact, or chiefly renamed it?',
    'Longitudinal comparison of marriage endogamy rates, hereditary ritual-office holdings, and manual-scavenging incidence inside reformist-governed institutions against matched non-reformist baselines.',
    'Genuine material reduction supports the medium-extraction reading; purely rhetorical reduction means theater_ratio is understated and the effective burden on payer seats is higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_versus_material_reduction, empirical, 'Whether the settlement''s softening of hierarchy is material or performative.').

omega_variable(
    internalized_suppression_share,
    'How much of the settlement''s hold on subordinated members is structural (kinship economics, village dependence, physical safety) versus internalized (acceptance of the spiritual-stages ordering)?',
    'Post-exit attitude trajectories among converts and second-generation diaspora: if ranked self-conceptions persist after structural ties are cut, a large share is internalized.',
    'A high internalized share means the scalar suppression figure understates the settlement''s total hold, since the suppression travels with the member after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_share, empirical, 'Structural versus internalized composition of the settlement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.18).
narrative_ontology:measurement(dhar_tr_t25, dharmasastra_corpus__reformist_contextual, theater_ratio, 25, 0.22).
narrative_ontology:measurement(dhar_tr_t50, dharmasastra_corpus__reformist_contextual, theater_ratio, 50, 0.27).
narrative_ontology:measurement(dhar_tr_t75, dharmasastra_corpus__reformist_contextual, theater_ratio, 75, 0.32).
narrative_ontology:measurement(dhar_tr_t100, dharmasastra_corpus__reformist_contextual, theater_ratio, 100, 0.37).
narrative_ontology:measurement(dhar_tr_t125, dharmasastra_corpus__reformist_contextual, theater_ratio, 125, 0.42).
narrative_ontology:measurement(dhar_tr_t150, dharmasastra_corpus__reformist_contextual, theater_ratio, 150, 0.45).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(dhar_be_t25, dharmasastra_corpus__reformist_contextual, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(dhar_be_t50, dharmasastra_corpus__reformist_contextual, base_extractiveness, 50, 0.54).
narrative_ontology:measurement(dhar_be_t75, dharmasastra_corpus__reformist_contextual, base_extractiveness, 75, 0.51).
narrative_ontology:measurement(dhar_be_t100, dharmasastra_corpus__reformist_contextual, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(dhar_be_t125, dharmasastra_corpus__reformist_contextual, base_extractiveness, 125, 0.46).
narrative_ontology:measurement(dhar_be_t150, dharmasastra_corpus__reformist_contextual, base_extractiveness, 150, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dhar_su_t25, dharmasastra_corpus__reformist_contextual, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(dhar_su_t50, dharmasastra_corpus__reformist_contextual, suppression_requirement, 50, 0.49).
narrative_ontology:measurement(dhar_su_t75, dharmasastra_corpus__reformist_contextual, suppression_requirement, 75, 0.45).
narrative_ontology:measurement(dhar_su_t100, dharmasastra_corpus__reformist_contextual, suppression_requirement, 100, 0.41).
narrative_ontology:measurement(dhar_su_t125, dharmasastra_corpus__reformist_contextual, suppression_requirement, 125, 0.38).
narrative_ontology:measurement(dhar_su_t150, dharmasastra_corpus__reformist_contextual, suppression_requirement, 150, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, caste_endogamy_marriage_norms).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, temple_priesthood_hereditary_access).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'Dharmasastra' decomposes, per the epsilon-invariance principle, into three structurally distinct claims — orthodox_literalist (eternal prescription), reformist_contextual (this file: separable ethical core, medium extraction, reduced victim set), and abolitionist_rejection (no legitimate authority remains). Each story carries its own epsilon, beneficiary/victim structure, and claimed type over the same referent; forcing one story to span all three would make epsilon observer-relative and violate DP-001. Family links run through affects_constraints in all three files. The reformist settlement is the hub: its institutional success changes the legitimacy conditions and resource availability of both siblings (orthodox pandits lose curricular ground; abolitionist movements gain the settled frame they organize against) without logically eliminating either — hence coexists_with edges rather than foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
