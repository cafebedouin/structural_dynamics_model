% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Reading: Direct Devotional Access Bypasses Caste Requirements
 *   domain: religious authority / social stratification / interpretive legitimacy
 *
 * SUMMARY:
 *   The devotional (bhakti) reading of the Vedic-Dharmic corpus holds that
 *   access to the divine is direct through devotion and that sincere devotion
 *   rather than birth determines spiritual authority. Historically it runs
 *   from the Tamil Alvar and Nayanar movements (c. 6th-9th centuries) through
 *   the medieval sampradayas and Sant traditions to the contemporary global
 *   devotional field. What it opened: religious participation without birth
 *   qualification, a canon of non-Brahmin and women saints, vernacular access
 *   to the sacred. What it built: teacher lineages (acharya paramparas),
 *   temple and matha networks, and a devotional economy of offerings,
 *   service, and deference. What persists: residual caste exclusion inside
 *   devotional institutions, succession practices that often track birth
 *   despite the anti-birth axiom, and a displaced hereditary establishment
 *   that adapted rather than vanished. Claim/metric independence is
 *   deliberate here: claimed_type rope is authored from the structural belief
 *   that this is genuine coordination — voluntary participation, unsuppressed
 *   alternatives, a real access function, no coercive enforcement holding it
 *   — while the metrics are authored from the descriptive belief that a
 *   moderate institutional levy accumulates inside it (extraction rising 0.20
 *   to 0.40 across the interval). The engine computes per-seat types from the
 *   structural data; where its verdicts diverge from the claim, that
 *   divergence is the measurement. KEY AGENTS (by structural relationship):
 *   see key_agents; the manifest's expected delta (moderate extraction, no
 *   concentrated beneficiary class at the level of the reading's core
 *   benefit, rope rather than snare, victim set shrunk but not eliminated) is
 *   refined in one respect — the access benefit is diffuse, but an
 *   institutional collector class (the teacher lineages) demonstrably
 *   captures the devotional economy's flows, which is recorded in the receipt
 *   surface rather than hidden.
 *
 * KEY AGENTS:
 *   - excluded_caste_devotees: primary dual seat (access beneficiary; residual-exclusion bearer) (moderate/constrained) — admitted by the devotional path, still partly gated inside its institutions
 *   - acharya_sampradaya_lineages: agenda-setting collector (institutional/identity_locked) — administers transmission and receives the devotional economy's flows
 *   - devotee_households: material base (moderate/mobile) — fund the institutions and discipline them by redirectable giving
 *   - women_devotees: dual seat (devotional voice gained; ritual office denied) (moderate/constrained)
 *   - caste_hindu_ritual_elites: displaced incumbent (institutional/constrained) — lost the interpretive monopoly; largely outside the devotional settlement
 *   - indological_scholars: analytical observer (analytical/analytical) — documents the arc from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.22).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Reading: Direct Devotional Access Bypasses Caste Requirements").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious authority / social stratification / interpretive legitimacy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, 'afb593fb-81f0-418d-afe5-20f3176806e4').
narrative_ontology:cs_kernel_codification('afb593fb-81f0-418d-afe5-20f3176806e4', fixed_text).
narrative_ontology:cs_authority_grounding('afb593fb-81f0-418d-afe5-20f3176806e4', lineage).
narrative_ontology:cs_interpretation_layer_present('afb593fb-81f0-418d-afe5-20f3176806e4').
narrative_ontology:cs_reading_relation('afb593fb-81f0-418d-afe5-20f3176806e4', vedic_dharmic_corpus__hereditary_monopoly_reading, influences).
narrative_ontology:cs_reading_relation('afb593fb-81f0-418d-afe5-20f3176806e4', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('afb593fb-81f0-418d-afe5-20f3176806e4', foundational, devotion_confers_spiritual_authority).
narrative_ontology:cs_axiom_status(devotion_confers_spiritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('afb593fb-81f0-418d-afe5-20f3176806e4', devotion_confers_spiritual_authority, theological).
narrative_ontology:cs_axiom('afb593fb-81f0-418d-afe5-20f3176806e4', foundational, divine_access_independent_of_birth).
narrative_ontology:cs_axiom_status(divine_access_independent_of_birth, holdable).
narrative_ontology:cs_axiom_grounding('afb593fb-81f0-418d-afe5-20f3176806e4', divine_access_independent_of_birth, theological).
narrative_ontology:cs_axiom('afb593fb-81f0-418d-afe5-20f3176806e4', secondary, vernacular_devotion_scripturally_valid).
narrative_ontology:cs_axiom_status(vernacular_devotion_scripturally_valid, holdable).
narrative_ontology:cs_axiom_grounding('afb593fb-81f0-418d-afe5-20f3176806e4', vernacular_devotion_scripturally_valid, conventional).
narrative_ontology:cs_reference_frame('afb593fb-81f0-418d-afe5-20f3176806e4', devotional_access_universal).
narrative_ontology:cs_drift_state('afb593fb-81f0-418d-afe5-20f3176806e4', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('afb593fb-81f0-418d-afe5-20f3176806e4', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, excluded_caste_devotees).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, women_devotees).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, devotee_households).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, acharya_sampradaya_lineages).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, excluded_caste_devotees).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, women_devotees).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, caste_hindu_ritual_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, devotee_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practice devotion directly — congregational singing, vernacular prayer, household and temple worship — without birth qualification or priestly mediation for access. The devotional path admitted them where the Vedic ritual order did not, and many canonized saints came from their ranks. Within many devotional institutions they still meet separate seating, denial of temple priesthood, and barred sanctum access, and lineage admission often tracks caste in practice. They can leave a devotional community, but the alternatives inside the religious field are birth-gated or demand renunciation, and their social world travels with them.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, excluded_caste_devotees, beneficiary,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, excluded_caste_devotees, payer).

% Head the teacher lineages that transmit devotional authority: initiate disciples, control temple networks and mathas, authorize vernacular commentary, and set practice norms. They receive offerings, service labor, and deference, and their succession chains — nominally grounded in the founder's realization — frequently pass to birth or adoption within the lineage. Their position and identity are the lineage itself; stepping outside it would dissolve the authority they hold.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, acharya_sampradaya_lineages, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, acharya_sampradaya_lineages, beneficiary).

% Give the devotional economy its material base: offerings, guru dakshina, festival and temple support, and service labor. They choose among sampradayas, gurus, and home practice, and that choice disciplines the institutions they support — a household can redirect its giving. What they receive is access, community, and a workable religious life; what they pay is the resources and labor that sustain the lineages and temples.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, devotee_households, payer,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, devotee_households, beneficiary).

% Gained devotional voice through the path — several canonized saints were women, and congregational practice admitted them where Vedic ritual largely did not. Inside most lineages and temples they still cannot hold ritual office, and their devotional authority is honored case by case rather than institutionalized. Exit from devotional communities is possible but carries social cost; their standing within them remains negotiated.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, women_devotees, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, women_devotees, payer).

% Hold ritual and interpretive authority by birth in the hereditary order the devotional path bypasses. They lost the interpretive monopoly over religious life as vernacular devotion spread — non-Brahmin saints were canonized, vernacular texts gained scriptural standing, and lay households found access without them. Many adapted by staffing devotional institutions or Sanskritizing vernacular theology; their standing remains tied to the birth-order they defend, and they were largely not parties to the devotional settlement that displaced them.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, caste_hindu_ritual_elites, excluded,
    institutional, generational, constrained, continental).

% Study the devotional movements, their texts, institutions, and social effects from outside participation. They document the original birth-gating of ritual access, the canonization of non-Brahmin and women saints, the institutionalization of teacher lineages, and the residual exclusion inside devotional spaces. Their seat carries no stake in the arrangement's persistence.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, indological_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__bhakti_devotional_reading, acharya_sampradaya_lineages).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__bhakti_devotional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the access problem of a birth-gated ritual order: it provides a religious participation path — vernacular practice, congregational worship, direct devotion — that requires neither Brahmin mediation, nor Sanskrit competence, nor birth qualification. It coordinates religious life across caste lines around shared devotional practice and transmits a portable practice set (chanting, singing, service) that anyone can adopt.
% TRANSFER_FUNCTION: Moves devotional labor (practice, singing, service), material resources (offerings, guru dakshina, temple and matha support), and deference from devotee households toward teacher lineages, temples, and exemplary devotees; and moves spiritual authority itself away from birth-qualified Brahmin lineages toward demonstrated devotion.
% ABSENT_VOICES: Dalit devotees inside devotional institutions — the settlement's egalitarianism is often proclaimed in their name while priesthood and sanctum decisions are made without them; women in lineages where ritual office remained closed; and the displaced hereditary establishment, which was largely not a party to the devotional settlement that displaced it. Each would contest the settlement's terms from a different direction.
% DISAPPEARANCE_RATIONALE: If direct devotional access and devotion-based authority vanished overnight, religious participation would re-gate on birth and priestly mediation: vernacular congregational life, the saints' canons, and lay households' religious practice would lose their operative frame, teacher lineages would lose their claim to authority, and the castes and women admitted by the devotional path would be pushed back to the ritual order's margins or out of religious life. The religious field's current shape depends on the arrangement.
% FOUNDING_PROBLEM: The Vedic ritual order gated religious participation and authority on birth into the twice-born varnas and on priestly mediation — those outside that order, and those without Sanskrit or ritual access, had no standing path to religious participation, scriptural access, or spiritual authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the displaced orthodoxy's own historical responses — medieval objections to vernacular authority, and in several documented cases persecution of devotional teachers — attest that the birth-gate was operative and that the devotional bypass was experienced as a breach of it; temple-entry movements and dalit devotional testimony attest the residual gate inside contemporary devotional institutions; academic indology, holding no stake in the arrangement, documents both the original gating and its residual forms.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.40 (moderate): the access benefit is real and broadly distributed, but the devotional economy transfers offerings, service labor, and deference to teacher lineages and temple institutions, the safety-valve dynamic collects transformative dissent, and residual caste exclusion persists inside devotional spaces. Suppression 0.22 (low): participation is voluntary; the hereditary ritual order, reformist critique, and renunciatory paths all remain live alternatives that the devotional arrangement does not suppress; institutional discipline inside sampradayas is real but mild. Suppression is authored as a raw structural property — only extractiveness is scaled by the engine's directionality and scope computation. Theater_ratio 0.28: the practice is largely functional — actual congregational life, actual access — with performative piety and guru-worship growing alongside institutionalization. Accessibility_collapse 0.30: understanding the devotional reading collapses no alternatives; the multiple paths (ritual, knowledge, action, devotion) coexist and the corpus sustains all of them. Resistance 0.40: the historical orthodoxy contested vernacular authority and in documented cases persecuted devotional teachers; contemporary dalit critique presses from inside; participants themselves resist little. All three measurement series share one eight-point grid (t0 = c. 600 CE, the Tamil devotional movements; t14 = c. 2000 CE; points at two-century intervals). suppression_requirement is tracked because the story's narrative specifically traces enforcement-capacity change: the early movements carried almost no enforcement machinery (0.08); sampradaya institutionalization built mild disciplinary capacity — lineage discipline, admission control, social pressure on dissenters (0.22 by interval end). base_extractiveness rises monotonically with institutionalization; theater_ratio rises with performative institutional piety; end-state values are consistent with base_properties at t14.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the acharya lineages' position the arrangement is a transmission they steward and an economy that sustains it — coordination they administer. From the excluded-caste seat the same arrangement is the door that opened and the corridor that still narrows: access gained, office denied. Devotee households experience voluntary giving they can redirect; women devotees experience voice without office; the displaced ritual elites experience usurpation of authority they hold by birth. Same arrangement, different experiences — the engine derives each seat's classification from power, exit, and directionality rather than averaging them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low end. acharya_sampradaya_lineages (declared beneficiary, identity-locked to the lineage) sit near the beneficiary pole — they collect, and cannot arbitrage away from the structure that pays them. devotee_households (beneficiary, mobile exit) sit nearest the beneficiary end — redirectable giving is the closest thing to arbitrage in the field. The dual-declared seats — excluded_caste_devotees and women_devotees, named in both the beneficiary and victim arrays — derive mid-range directionality weighted toward the beneficiary side, because the access gain dominates the residual exclusion they bear. caste_hindu_ritual_elites are declared victims with constrained exit: high directionality toward target, an institutional actor bearing the arrangement's costs (the displacement of their authority is the arrangement working as intended, but it is a cost they structurally bear). No directionality overrides are authored: the beneficiary/victim declarations plus exit options already separate the seats, and the override mechanism is keyed per power atom, which could not distinguish the two institutional actors (the lineages and the elites) from each other anyway.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — birth-gated access to religious participation and authority — persists in attenuated form: caste still shapes temple priesthood, sanctum access, and lineage admission, so the arrangement's function is live and no mandatrophy is declared. The classification work the metrics do: reading the arrangement as pure coordination would miss the extraction accumulating in its institutional layer (the base_extractiveness series rises 0.20 to 0.40 as lineages and temple economies mature); reading it as pure extraction would miss that participation is voluntary, alternatives are unsuppressed, and the victim set is residual rather than structural to the arrangement's operation. The honest middle is a coordination arrangement carrying a growing institutional levy — which is what the moderate, slowly rising series records.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_valve_vs_transformation,
    'Is the devotional path''s historical social function a genuine bypass of caste requirements, or a safety valve that preserved caste by channeling egalitarian religious energy away from social transformation?',
    'Comparative historical analysis of caste rigidity and social mobility in regions and periods of strong institutionalized devotional movements versus weak presence; dalit movement historiography on whether devotional inclusion preceded or substituted for social inclusion.',
    'If the safety-valve function dominates, the arrangement''s effective extraction is higher than the authored 0.40 (it collects transformative dissent) and the classification tilts toward a hybrid coordination/extraction profile; if transformation is real, the liberatory function stands and the current values hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_valve_vs_transformation, empirical, 'Whether the devotional bypass transformed caste or stabilized it.').

omega_variable(
    succession_hereditary_rebound,
    'Does devotional authority re-institutionalize into a birth-like hierarchy — do acharya successions pass by lineage birth or adoption rather than demonstrated devotion?',
    'Longitudinal study of sampradaya succession records: the fraction of successions passing to birth or adopted heirs versus devotees selected for realized practice, compared across lineages and periods.',
    'If succession is largely hereditary in practice, the reading''s anti-birth axiom is contradicted by its own institutions, effective extraction rises, and the residual hierarchy is deeper than the residual-exclusion measure captures; if devotion-based succession holds, the reference frame is intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_hereditary_rebound, empirical, 'Whether the anti-birth axiom survives its own institutional succession practice.').

omega_variable(
    residual_exclusion_extent,
    'How extensive is caste exclusion inside contemporary devotional institutions — temple priesthood, sanctum access, lineage admission, congregational seating?',
    'Institutional audit across sampradayas and temples: appointment records for ritual office, sanctum access policy, admission practice; triangulated with dalit devotional testimony.',
    'A wide extent raises the victim side of the ledger and the effective extraction borne by excluded_caste_devotees beyond what the dual beneficiary/victim declaration captures; a narrow extent supports the access-gain-dominates reading and the current moderate values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_exclusion_extent, empirical, 'Size of the residual caste exclusion inside devotional institutions.').

omega_variable(
    authority_framing_lineage_vs_practice,
    'Should the reading''s authority be framed as lineage-grounded (sampradaya transmission chains adjudicate the corpus) or practice-grounded (realized devotion adjudicates, with the anti-institutional strands of the tradition — Kabir, Basava, the Sant lineages — as the pure case)?',
    'No empirical resolution; the framing choice follows from which instantiation is taken as definitional — the institutional sampradayas that dominate the standing arrangement, or the saint strands that rejected institutional mediation. Signals for the lineage framing chosen here: institutional dominance, succession structures, temple control.',
    'Under the practice framing, the acharya lineages lose their adjudicating seat, institutional collection reads as voluntary rather than structural, and measured extraction falls; under the lineage framing, the authored values hold. The two framings can classify the same practice differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_lineage_vs_practice, conceptual, 'Framing under-determination: lineage-instantiated versus practice-instantiated authority.').

omega_variable(
    kernel_reading_commitment,
    'This story instantiates one reading — the devotional reading — of the shared Vedic-Dharmic corpus kernel; what would the sibling readings (hereditary monopoly, reformist egalitarian) change structurally, and where is the disagreement located?',
    'No in-story resolution: the readings are commitments, not competing measurements of one quantity. Corpus-level comparison requires the sibling stories. The disagreement is located in the source-of-authority premise (birth versus devotion versus rational critique) and in whether varna gating is scriptural essence or historical accretion.',
    'A sibling reading changes the victim set (the hereditary reading''s victims are all outside the twice-born order; the reformist reading''s are those the hierarchy subordinates), the beneficiary structure (concentrated in Brahmin lineages versus diffuse), and the classification of the same corpus. This story''s verdict applies only to the devotional reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bhakti_devotional_reading_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(bhakti_devotional_reading_tr_t0, observed).
narrative_ontology:measurement(bhakti_devotional_reading_tr_t2, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement_basis(bhakti_devotional_reading_tr_t2, observed).
narrative_ontology:measurement(bhakti_devotional_reading_tr_t4, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement_basis(bhakti_devotional_reading_tr_t4, observed).
narrative_ontology:measurement(bhakti_devotional_reading_tr_t6, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(bhakti_devotional_reading_tr_t6, observed).
narrative_ontology:measurement(bhakti_devotional_reading_tr_t8, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(bhakti_devotional_reading_tr_t8, observed).
narrative_ontology:measurement(bhakti_devotional_reading_tr_t10, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(bhakti_devotional_reading_tr_t10, observed).
narrative_ontology:measurement(bhakti_devotional_reading_tr_t12, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(bhakti_devotional_reading_tr_t12, observed).
narrative_ontology:measurement(bhakti_devotional_reading_tr_t14, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement_basis(bhakti_devotional_reading_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(bhakti_devotional_reading_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(bhakti_devotional_reading_be_t0, observed).
narrative_ontology:measurement(bhakti_devotional_reading_be_t2, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 2, 0.24).
narrative_ontology:measurement_basis(bhakti_devotional_reading_be_t2, observed).
narrative_ontology:measurement(bhakti_devotional_reading_be_t4, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement_basis(bhakti_devotional_reading_be_t4, observed).
narrative_ontology:measurement(bhakti_devotional_reading_be_t6, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement_basis(bhakti_devotional_reading_be_t6, observed).
narrative_ontology:measurement(bhakti_devotional_reading_be_t8, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement_basis(bhakti_devotional_reading_be_t8, observed).
narrative_ontology:measurement(bhakti_devotional_reading_be_t10, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement_basis(bhakti_devotional_reading_be_t10, observed).
narrative_ontology:measurement(bhakti_devotional_reading_be_t12, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement_basis(bhakti_devotional_reading_be_t12, observed).
narrative_ontology:measurement(bhakti_devotional_reading_be_t14, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 14, 0.4).
narrative_ontology:measurement_basis(bhakti_devotional_reading_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(bhakti_devotional_reading_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(bhakti_devotional_reading_su_t0, observed).
narrative_ontology:measurement(bhakti_devotional_reading_su_t2, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 2, 0.1).
narrative_ontology:measurement_basis(bhakti_devotional_reading_su_t2, observed).
narrative_ontology:measurement(bhakti_devotional_reading_su_t4, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 4, 0.12).
narrative_ontology:measurement_basis(bhakti_devotional_reading_su_t4, observed).
narrative_ontology:measurement(bhakti_devotional_reading_su_t6, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 6, 0.15).
narrative_ontology:measurement_basis(bhakti_devotional_reading_su_t6, observed).
narrative_ontology:measurement(bhakti_devotional_reading_su_t8, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 8, 0.17).
narrative_ontology:measurement_basis(bhakti_devotional_reading_su_t8, observed).
narrative_ontology:measurement(bhakti_devotional_reading_su_t10, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(bhakti_devotional_reading_su_t10, observed).
narrative_ontology:measurement(bhakti_devotional_reading_su_t12, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement_basis(bhakti_devotional_reading_su_t12, observed).
narrative_ontology:measurement(bhakti_devotional_reading_su_t14, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 14, 0.22).
narrative_ontology:measurement_basis(bhakti_devotional_reading_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% The Vedic-Dharmic corpus is a single kernel read three ways; each reading is a separate constraint with its own epsilon, beneficiary/victim structure, and classification. This file authors the devotional reading only (epsilon ~0.40, moderate extraction concentrated in the institutional layer, access benefit broadly distributed). The hereditary monopoly reading authors the birth-gated arrangement (higher epsilon, concentrated Brahmin-lineage beneficiaries); the reformist egalitarian reading authors the constitutional-conformity arrangement. The devotional reading structurally influences both siblings: it eroded the hereditary monopoly's legitimacy conditions without foreclosing it, and it supplied the reformist reading's resource base (the canon of non-Brahmin and women saints, vernacular textual access) without resolving the reformist dispute. The three stories form one constraint family and must be read together for any corpus-level verdict.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
