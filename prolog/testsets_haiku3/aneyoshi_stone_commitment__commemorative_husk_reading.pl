% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone: Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   In 1933, the village of Aneyoshi erected a stone marker at approximately
 *   23 meters above sea level, with an inscription warning that future
 *   residents should not build below this line or face tsunamis. Under the
 *   behavioral-competence reading, this stone functioned as an operational
 *   land-use constraint: builders consulted it, made decisions, and the
 *   village's survival in the 2011 Tōhoku tsunami vindicates its causal
 *   efficacy. Under the commemorative-husk reading instantiated here, the
 *   stone's directive force decayed to symbolic observance across 78 years:
 *   land-use decisions were governed by economic proximity to the coast,
 *   municipal zoning codes, and social convention — the stone became a museum
 *   piece, a narrative artifact whose survival and prominence in post-2011
 *   reporting reflects retrospective meaning-making, not contemporaneous
 *   behavioral competence. The 2011 survival is reframed in this reading:
 *   Aneyoshi's homes happened to align with the historical guideline, but
 *   modern residents did not consult the stone when deciding to stay; the
 *   stone's presence became evidence of ancestor wisdom only AFTER the
 *   survival was assured. This reading is one contest over what the stone IS
 *   NOW and what it WAS DOING across its institutional lifespan.
 *
 * KEY AGENTS:
 *   - aneyoshi_residents_contemporary: Urban dwellers 2011-2026, inheritors of village location and stone narrative but not active custodians of its directive function. They carry the stone's fame but make land-use decisions through insurance, zoning codes, and job proximity.
 *   - aneyoshi_residents_1933_1978: Original community and their descendants (1933-2011). Under the behavioral reading, they are the interpretation's anchor (they obeyed the stone). Under the commemorative reading, they are a retroactively projected audience — actual decision-making records are unavailable, so their agency toward the stone is inferred rather than documented.
 *   - cultural_institutions_researchers: Anthropologists, memorial keepers, journalists, and disaster-response officials who transformed the stone into an interpretive artifact after 2011. They are the primary agents keeping the stone's narrative alive, whether as proof of indigenous knowledge or as museum piece.
 *   - municipal_government_aneyoshi: Contemporary governance body managing land use and memorial status. They could enforce the stone's guideline as a code requirement; they do not, treating it instead as cultural heritage requiring preservation but not behavioral constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.02).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.92).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.92).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Stone: Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '041637a2-4d3c-4ce4-984b-91de79a59bbe').
narrative_ontology:cs_kernel_codification('041637a2-4d3c-4ce4-984b-91de79a59bbe', fixed_text).
narrative_ontology:cs_authority_grounding('041637a2-4d3c-4ce4-984b-91de79a59bbe', lineage).
narrative_ontology:cs_interpretation_layer_present('041637a2-4d3c-4ce4-984b-91de79a59bbe').
narrative_ontology:cs_reading_relation('041637a2-4d3c-4ce4-984b-91de79a59bbe', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('041637a2-4d3c-4ce4-984b-91de79a59bbe', foundational, stone_directive_functionally_inoperative).
narrative_ontology:cs_axiom_status(stone_directive_functionally_inoperative, holdable).
narrative_ontology:cs_axiom_grounding('041637a2-4d3c-4ce4-984b-91de79a59bbe', stone_directive_functionally_inoperative, empirically_contingent).
narrative_ontology:cs_axiom('041637a2-4d3c-4ce4-984b-91de79a59bbe', foundational, survival_alignment_is_accident_not_causation).
narrative_ontology:cs_axiom_status(survival_alignment_is_accident_not_causation, holdable).
narrative_ontology:cs_axiom_grounding('041637a2-4d3c-4ce4-984b-91de79a59bbe', survival_alignment_is_accident_not_causation, empirically_contingent).
narrative_ontology:cs_reference_frame('041637a2-4d3c-4ce4-984b-91de79a59bbe', ancestor_wisdom_operational_across_generations).
narrative_ontology:cs_drift_state('041637a2-4d3c-4ce4-984b-91de79a59bbe', contemporary_museum_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('041637a2-4d3c-4ce4-984b-91de79a59bbe', '2026-06-12T14:33:22Z').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_researchers_anthropologists).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_government_aneyoshi).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, media_and_global_narrative_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the village of Aneyoshi, 2011-present. Their location choice is governed by employment proximity to the coast, property prices, and social ties to the village — not by consultation with the stone. They are aware of the stone's presence and the post-2011 narrative about ancestor wisdom, but the stone does not constrain their behavior or their exit options. They can leave Aneyoshi at any time with no reference to the stone.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_contemporary_residents, observer,
    moderate, biographical, mobile, local).

% Maintain the stone as a cultural artifact and interpretive site. They organize annual commemorations, guide researchers and tourists, and produce narratives about indigenous disaster knowledge. The stone's preservation and symbolic prominence are their administrative responsibility. They would experience material and reputational cost if the stone were moved or its narrative collapsed, but they do not enforce any land-use compliance or extract economic rent from it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, cultural_heritage_institutions, agenda_setter,
    organized, generational, constrained, regional).

% Study the stone as evidence of indigenous disaster-response systems and pre-scientific community knowledge. The stone's existence and the post-2011 survival narrative provide research material, publication opportunities, and career advancement. They benefit from the stone's prominence in global discourse about climate resilience and traditional knowledge, but they do not enforce the stone's directive or bear cost from land-use compliance.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_researchers_anthropologists, beneficiary,
    organized, biographical, arbitrage, global).

% Administers village governance and land-use permitting. They preserve the stone as cultural heritage and promote it for tourism, generating modest revenue and cultural prestige. They do not enforce the stone's elevation guideline in building codes — zoning and safety are governed by municipal statute and national regulations. The stone's narrative value outweighs any pressure to make it an operative constraint.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_government_aneyoshi, agenda_setter,
    institutional, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_government_aneyoshi, beneficiary).

% The stone's story — 'ancient Japanese village heeds ancestor wisdom, survives 2011 tsunami' — is a powerful narrative asset for media, textbook publishers, and disaster-resilience discourse globally. The story is economically valuable (it circulates as inspirational content, academic case studies, and tourism marketing). The apparatus has no formal enforcement role but benefits from the stone's symbolic prominence and the narrative interpretability it offers.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, media_and_global_narrative_apparatus, beneficiary,
    institutional, biographical, arbitrage, global).

% Historical actors now deceased who may or may not have consulted the stone in building decisions 1933-2011. Under the behavioral reading, they are the central agents; under the commemorative reading, their agency is speculative — no records exist of whether they actually treated the stone as an operational constraint or knew of it at all. They are excluded from contemporary decision-making and cannot testify to their own land-use reasoning.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents_1933_1978, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the commemorative reading, there is no live coordination function. The stone was intended in 1933 to coordinate future-community building decisions around tsunami risk, but this function has decayed. Contemporary coordination around disaster risk is handled by municipal building codes, insurance markets, and national disaster-response systems independent of the stone.
% TRANSFER_FUNCTION: No economically measurable transfer occurs under this reading. The stone does not extract value from land users or builders. The transfer is symbolic: cultural narrative and researcher attention flow toward the stone (from the global discourse apparatus), generating prestige and research material that accrue to heritage institutions and anthropologists. But this is not extraction in the constraint-theory sense because no actor paying attention to the stone is constrained by it.
% ABSENT_VOICES: The contemporary land-use decision-makers (property owners, developers, municipal planners) do not consult the stone and are not in conversation about it when making decisions. If they were here, they would say: 'We do not use the stone's guideline; we use building codes. The stone is interesting for tourism but not for safety.' The ancestors (1933 community) are also absent — their actual intentions and decision-making processes are unrecorded, so any claim about their stone-consultation behavior is inference, not testimony.
% DISAPPEARANCE_RATIONALE: If the stone disappeared tomorrow, Aneyoshi's land-use patterns would not change — residents would continue their coastal living in the same structures, governed by the same economic and regulatory pressures. However, the village would lose a powerful cultural asset (tourism value, narrative identity, research interest) that carries prestige and modest economic benefit. The disappearance would be rearrangement at the symbolic and cultural-identity level, not at the behavioral or safety level. Some parties (heritage institutions, global narrative apparatus) would experience loss; land users would experience none. The contest arises because some readings treat the stone as functionally inert (world_unchanged if it vanishes) while others treat it as constitutive of post-2011 meaning-making about resilience (world_rearranges at the identity level).
% FOUNDING_PROBLEM: The 1933 Aneyoshi community erected the stone to warn future generations not to build below a certain elevation, having observed or theorized a tsunami risk from the Pacific. The founding problem was: how does a community encode long-term disaster knowledge in a form that will survive institutional decay and still shape behavior 78+ years later? The stone was an intervention in the temporal problem of knowledge transmission across generations.
% FOUNDING_PROBLEM_CORROBORATION: Under the commemorative reading, the founding problem is dead because its solution failed. The stone could not encode knowledge in a form that persisted as behavioral competence. The contemporary problem of disaster-risk communication is now solved (or addressed) by building codes, scientific monitoring, and insurance markets — not by ancestor stones. No actor outside heritage institutions attests that the stone's original problem is still live. Researchers and journalists attested (post-2011) that the stone's survival created a narrative about indigenous knowledge, but this is a different problem — a post-hoc meaning-making exercise, not the original 1933 intention. Even heritage institutions do not argue that the stone is still functionally encoding disaster knowledge for contemporary land-use decisions; they argue that it is a memorial to a past attempt at knowledge transmission.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).
:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under the commemorative-husk reading, extractiveness is near-zero (0.05 terminal) because the constraint does not govern land use; residents do not bear a cost from obeying a directive they do not follow. The stone collects no rents, imposes no burden, and operates at the symbolic level only. Suppression is negligible (0.02) because there is no resistance to suppress — the stone's non-function is not contested by those making land-use decisions. Theater is very high (0.92 terminal) because the constraint's entire observable existence is performative: annual commemorations, tourism, academic papers on indigenous disaster knowledge, and the stone's physical maintenance are all acts of meaning-making that do not alter land-use behavior. The measurement series shows theater rising monotonically as the stone transitions from dormant artifact (1933-2011) to media-saturated memorial (2011-present), while extractiveness decays because no one ever actually made land-use decisions based on the stone's directive in the contemporary era. This pattern — rising theater, falling extractiveness — is the diagnostic signature of a piton: institutional persistence driven by symbolic maintenance and narrative utility rather than by functional necessity or concentrated benefit.
 *
 * PERSPECTIVAL GAP:
 *   The behavioral-competence reading and the commemorative-husk reading should compute into very different types from the same named artifact. From the behavioral seat (builders consulting the stone, respecting the 1933 community judgment), the stone is a rope or tangled-rope: genuine coordination solved a collective-action problem (where to build safely), and compliance was real. From the commemorative seat (post-2011 researchers, memorial keepers, symbolic audiences), the stone is a piton: its original coordination function has atrophied, and it persists through institutional inertia and cultural-heritage narratives. The engine's per-seat computation should produce this divergence from the structural data: the behavioral reading would author high accessibility_collapse and low resistance (alternatives collapse when the danger is real; resistance is absent because the stone's guideline aligns with self-interest). The commemorative reading authors low accessibility_collapse (alternatives are not closed; residents have many ways to decide where to build) and low resistance (there is no resistance because no one is being extracted from or constrained by a non-operative directive).
 *
 * DIRECTIONALITY LOGIC:
 *   There are no structural beneficiaries and no victims under this reading because the constraint does not constrain. The stone's symbolic presence benefits cultural narratives about indigenous wisdom and Japanese disaster resilience, but these narratives are not agents; they are vindicated propositions. Contemporary land-use decisions are decoupled from the stone's directive, so residents do not bear directionality d toward the constraint — they simply ignore it. The municipal government holds a light stake (ceremonial preservation duties, tourist attraction value) but not asymmetric extraction or coordination. The absence of beneficiaries/victims reflects the reading's core claim: the constraint has become inert as a behavioral mechanism and persists as pure theater.
 *
 * MANDATROPHY ANALYSIS:
 *   The commemorative-husk reading resolves the mandatrophy question by rejecting its premise: the stone's founding mandate (warn future generations not to build below the line) is not obsolete — it is simply inoperative. The mandate persists in inscription; compliance persists as zero. This is not a case of a coordination problem being solved and the constraint outliving its use. Rather, it is a case of a constraint that was always institutional-aspirational (a plea from the 1933 community to unknown futures) becoming unmoored from the behavioral world it addressed. The 2011 survival does not vindicate the stone's causal efficacy under this reading; it is pure accident — a alignment of historical geography with contemporary residential patterns, nothing more. The piton classification captures this structure: the stone's administrative status (preserved, commemorated, studied) is maintained by cultural institutions and memorial practices, not by any actor benefiting from its operation or any actor enforcing its compliance. If the narrative utility fades (if the stone is demolished, if the tsunami memory recedes), the constraint will dissolve because no concentrated actor depends on it. The mandate has become a text floating free of behavioral linkage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_decay_vs_fabricated_narrative,
    'Did the stone''s land-use guidance actually decay into symbol, or was the ''behavioral competence'' interpretation always a post-hoc narrative imposed after the 2011 survival event?',
    'Archival examination of pre-2011 land-use records: did builders consult the stone''s elevation guideline when making decisions 1933-2011, or was the stone already a dormant artifact that the survival event retrospectively activated as proof of ancestor wisdom?',
    'If functional decay is real (behavioral reading was once live, became theater over time), this reading is a chronicle of institutional atrophy. If the behavioral competence narrative was always post-hoc rationalization, both readings are stories about meaning-making after the fact, and the constraint''s structural novelty is different — it would be an inversion omega, not a decay omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_decay_vs_fabricated_narrative, empirical, 'Whether the stone''s directive guidance was ever operationally binding or was always memorialized.').

omega_variable(
    agency_bifurcation_across_readings,
    'In the behavioral reading, is the stone''s causal agency attributed to the builders'' rational choice to follow its elevation rule, or to ancestor wisdom encoded supernaturally in stone? In the commemorative reading, what becomes of that agency claim?',
    'Interview cohort of contemporary Aneyoshi residents and descendant-builders: what do they say the stone WAS doing in 1933? What do they say it IS doing in 2026? How do they account for the 2011 survival?',
    'If the behavioral reading attributes the stone to rational builders'' choice and the commemorative reading revokes that agency, the readings are incompatible only on the question of causation, not on facts. If both readings agree the stone carried agency but the commemorative reading reassigns it from ''guiding builders'' to ''watching from memory,'' the readings coexist semantically but diverge on the stone''s contemporaneous role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_bifurcation_across_readings, conceptual, 'How the two readings construct the stone''s causal and normative agency.').

omega_variable(
    memorial_husk_suppression_mechanism,
    'Is the commemorative reading''s theater ratio (0.92) capturing real performance — maintenance rituals, annual readings, curatorial labor — or is the high value an artifact of collapsing all symbolic work into the measurement category?',
    'Time-budget study: measure hours spent maintaining the stone physically vs. hours spent interpreting/displaying it. Measure land-use compliance attributable to the stone''s directive vs. compliance to municipal codes independent of the stone.',
    'If the theater is real maintenance work, the piton classification is appropriate. If the high theater ratio reflects measurement bias (treating all symbolic recognition as non-functional), the suppression mechanism may be different — the stone may be constrained by deliberate institutional forgetting rather than by performative memory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_husk_suppression_mechanism, empirical, 'Whether the memorial function is active theater or passive neglect mislabeled as remembrance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 0, 52).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.75).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 13, 0.81).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 26, 0.87).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 39, 0.9).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 52, 0.92).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 13, 0.06).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 26, 0.04).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 39, 0.03).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 52, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(aney_su_t13, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 13, 0.03).
narrative_ontology:measurement(aney_su_t26, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 26, 0.02).
narrative_ontology:measurement(aney_su_t39, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 39, 0.02).
narrative_ontology:measurement(aney_su_t52, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 52, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_commitment kernel decomposes into two structurally distinct constraints: behavioral_competence_reading treats the stone as an operational land-use rule retaining causal force across 78 years (likely: rope or tangled_rope); commemorative_husk_reading treats the stone as a symbolic memorial whose directive function decayed to theater (piton). These are not two measurements of the same constraint; they are two different constraints emitted from the same contested kernel. The epsilon value differs substantially: behavioral reading would author ε near 0.4-0.6 (genuine coordination with compliance costs); commemorative reading authors ε near 0.05 (no behavioral constraint, symbolic observance only). The network edge indicates that the behavioral reading's epistemic claim (the stone guides builders) is upstream of the commemorative reading's epistemic claim (the stone is now a museum piece) — the behavioral claim would have to be false or obsolete for the commemorative reading to be structurally defensible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
