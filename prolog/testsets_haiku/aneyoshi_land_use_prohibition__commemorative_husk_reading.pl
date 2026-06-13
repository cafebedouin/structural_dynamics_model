% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Land-Use Prohibition (Commemorative Husk Reading)
 *   domain: disaster_anthropology/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   The Aneyoshi stone marks a land-use prohibition established by the
 *   historical Aneyoshi community in response to a catastrophic tsunami. In
 *   the behavioral_competence_reading, the prohibition is a working rule:
 *   enforced through community practice, taught to each generation,
 *   operationally binding across 78 years. In THIS reading (commemorative
 *   husk), the prohibition has decayed to a cultural symbol. The stone is
 *   maintained, ceremonies are held, children are taught its history — but
 *   development routinely proceeds in the prohibited zone. The constraint
 *   persists as theatrical memorial while its behavioral content has
 *   evaporated. Development interests benefit from this decay; future
 *   residents below the high-water mark become unaware victims when they
 *   settle structures in the zone the original community marked as
 *   catastrophically risky. The claim (piton) and the metrics (high
 *   extractiveness, high theater ratio, low suppression requirement) are
 *   authored independently: this is a constraint that LOOKS like it persists
 *   through cultural reverence but actually persists through the decay of its
 *   binding force into symbol, enabling extraction by development interests.
 *   The engine will compute per-seat types; the marked divergence between the
 *   behavioral_competence_reading's operative prohibition and this reading's
 *   commemorative symbol is where the analytical leverage sits.
 *
 * KEY AGENTS:
 *   - aneyoshi_villagers_historical: Original rule-setters; dead; enforced prohibition through 78 years of continuous community practice
 *   - development_interests: Beneficiaries; treat stone as cultural artifact whose ceremonial maintenance provides reputation cover for zone development
 *   - contemporary_civic_authorities: Agenda-setters for the constraint's preservation; frame it as heritage, not operative rule; issue development permits in prohibited zone
 *   - future_residents_below_high_water_mark: Victims; powerless and trapped; will bear catastrophic loss when disaster strikes developed zone
 *   - disaster_risk_scientists: Observers; document the decay of operative knowledge into ceremonial artifact; flag the mismatch
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.78).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.32).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Land-Use Prohibition (Commemorative Husk Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, '5da9d956-cd8c-44f9-8c46-19def54a9dec').
narrative_ontology:cs_kernel_codification('5da9d956-cd8c-44f9-8c46-19def54a9dec', fixed_text).
narrative_ontology:cs_authority_grounding('5da9d956-cd8c-44f9-8c46-19def54a9dec', extraction).
narrative_ontology:cs_reading_relation('5da9d956-cd8c-44f9-8c46-19def54a9dec', aneyoshi_land_use_prohibition__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('5da9d956-cd8c-44f9-8c46-19def54a9dec', foundational, prohibition_exists_as_cultural_artifact).
narrative_ontology:cs_axiom_status(prohibition_exists_as_cultural_artifact, holdable).
narrative_ontology:cs_axiom_grounding('5da9d956-cd8c-44f9-8c46-19def54a9dec', prohibition_exists_as_cultural_artifact, conventional).
narrative_ontology:cs_axiom('5da9d956-cd8c-44f9-8c46-19def54a9dec', foundational, modern_building_codes_supersede_historical_precautions).
narrative_ontology:cs_axiom_status(modern_building_codes_supersede_historical_precautions, holdable).
narrative_ontology:cs_axiom_grounding('5da9d956-cd8c-44f9-8c46-19def54a9dec', modern_building_codes_supersede_historical_precautions, empirically_contingent).
narrative_ontology:cs_reference_frame('5da9d956-cd8c-44f9-8c46-19def54a9dec', ceremonial_preservation_framework).
narrative_ontology:cs_drift_state('5da9d956-cd8c-44f9-8c46-19def54a9dec', contemporary_development_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5da9d956-cd8c-44f9-8c46-19def54a9dec', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, civic_commemoration_narrative).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_high_water_mark).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, historical_memory_keepers).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__commemorative_husk_reading, historical_memory_as_sufficient_risk_governance).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__commemorative_husk_reading, symbolic_continuation_replaces_behavioral_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established the original prohibition in response to a catastrophic tsunami. Set the high-water mark stone at a boundary deemed safe after observation of the disaster. Enforced the rule through community practice and social sanctions across 78 years of continuous residence below the mark. The prohibition was a binding land-use rule backed by memory of specific deaths and explicit teaching to children.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_villagers_historical, agenda_setter,
    moderate, generational, constrained, local).

% Maintain the stone as a historical marker and commemorative object. Preserve its text and location. Frame it as cultural heritage and educational symbol. Do not treat it as an operative land-use rule; permits for development in the prohibit zone are issued routinely. The stone is theatrically honored — ceremonies, markers, educational signage — while its behavioral content is ignored.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, contemporary_civic_authorities, agenda_setter,
    institutional, biographical, mobile, regional).

% Gain economically from treating the prohibition as a historical curiosity rather than an operative rule. Can site real estate, commercial structures, and infrastructure in the zone designated as high-risk by the original community. Benefit from the symbolic maintenance of the stone (reputation cost of demolishing it) while capturing the zone's development value. Exit via regulatory capture or reframing the memory as purely ceremonial.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests, beneficiary,
    powerful, biographical, arbitrage, regional).

% Will inhabit structures built in the historically designated high-risk zone without knowledge of the prohibition or its original behavioral force. Bear the catastrophic cost when the next disaster-scale tsunami arrives and finds inhabited development in the zone the original community explicitly marked as unsafe. Their exit option is zero — they are not present when the decision is made and have no mechanism to reject the developed landscape once they arrive.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_high_water_mark, payer,
    powerless, biographical, trapped, local).

% Benefit from the stone's symbolic function as a monument to disaster memory and cultural continuity. Use it as an educational artifact and proof of historical consciousness. The stone's existence as a commemorative object vindicates the narrative that the community was wise and the memory is preserved. Do not benefit if the prohibition is re-operationalized, which would shift the stone from symbol to working rule.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, historical_memory_keepers, beneficiary,
    moderate, generational, constrained, local).

% Track tsunami risk and land-use patterns across coastal regions. Recognize the Aneyoshi stone as evidence of historical risk knowledge but note that its communicative force has degraded from operative rule to cultural artifact. Observe the mismatch between the zone's historical designation and contemporary development patterns. Can measure the drift and flag the reversal of behavioral competence.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_planning_authorities, observer,
    institutional, generational, analytical, regional).

% Study how institutional memory of natural hazards persists or decays. The Aneyoshi case exemplifies the transition from operative folk knowledge (behavioral rule enforced by community) to ceremonial artifact (honored symbolically while functionally neutralized). Can document the mechanism of decay and the conditions enabling it.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_risk_scientists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this reading denies coordination function. In the behavioral_competence_reading, the prohibition coordinates land-use across generations. In THIS reading, the stone is a one-way memorial, not a multi-party agreement. The original community that created the prohibition is dead; contemporary actors treat it as a ceremonial object, not a binding rule.
% TRANSFER_FUNCTION: Temporal transfer of risk and benefit. Development interests capture present economic value from building in the high-risk zone. The stone's ceremonial maintenance provides reputational cover (cultural reverence) for the development. Future residents, not yet present or informed, bear the tail-risk cost when the next tsunami arrives. The constraint transfers catastrophic future loss to parties who did not authorize the zone's occupation.
% ABSENT_VOICES: The original Aneyoshi community (dead, but their warning stands in stone). Disaster-risk scientists argue for re-operationalizing the prohibition but are marginalized in development and civic authority deliberations. Future residents below the high-water mark cannot object to decisions made before they arrive. Preservation advocates who recognize the prohibition as a live warning, not a relic, are drowned out by development-friendly authorities and the commemorative narrative.
% DISAPPEARANCE_RATIONALE: If the stone's symbolic maintenance were to end (demolition or neglect), surface-level outcomes would be unchanged — development in the zone would continue as it does now. But the stone's disappearance would expose the constraint's true state: the prohibition has no behavioral force, only a ceremonial presence. The real rearrangement occurs when the next disaster arrives and strikes a developed zone that the stone warned was unsafe. The constraint's decay becomes catastrophically apparent only through the absence of its warning at the moment it is needed.
% FOUNDING_PROBLEM: A catastrophic tsunami killed many Aneyoshi residents. The survivors established a prohibition against settlement below a high-water mark and enforced it through continuous community practice and intergenerational teaching, embedding the rule in local knowledge and social sanctions.
% FOUNDING_PROBLEM_CORROBORATION: The geophysical founding problem (tsunami risk) is LIVE. The GOVERNANCE founding problem (how to transmit and enforce safe settlement boundaries across generations) is DEAD in this reading, according to civic authorities and development interests. They testify that modern disaster science, building codes, and engineering standards have superseded the need for community-enforced land-use rules. Disaster-risk scientists dispute this, atesting that the Aneyoshi prohibition worked and has decayed, and that modern codes do NOT actually constrain development in the zone. No external corroborator from outside the development-friendly coalition attests that the founding problem is actually solved — only that it is no longer treated as a live problem by authorities managing the constraint.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising across the interval (0.08 → 0.78) because development interests systematically capture the economic value of the zone while the stone's symbolic maintenance provides reputational cover and cultural legitimacy for the capture. The constraint is a piton: the original behavioral rule has atrophied, but the stone persists due to institutional inertia and theatrical maintenance (ceremonies, educational signage). Theater ratio is very high (0.81 at endpoint) because the dominant activity around the stone is ceremonial and commemorative, not functional — the actual land-use decisions in the zone are made independently of the prohibition. Suppression requirement is LOW and falling (0.85 → 0.32) because no one is actively defending the prohibition as a rule; its decay requires no enforcement machinery to sustain. The historical community enforced the rule through social sanctions and teaching (high suppression_requirement in the behavioral_competence_reading); this reading's contemporary authorities enforce nothing — they merely maintain the symbol. Accessibility collapse is low (0.42) because alternatives to the prohibition (modern building codes, scientific risk assessment, development) remain institutionally salient even though they fail to prevent settlement in the high-risk zone. Resistance is high (0.71) because disaster-risk scientists, some preservation advocates, and the ghost of the original prohibition's intent all resist the zone's development — but resistance is ineffective under the theatrical framing. The measurements chart the transition: extractiveness accumulates as development proceeds; theater ratio rises as the stone shifts from operative rule to monument; suppression requirement falls as enforcement activity evaporates.
 *
 * PERSPECTIVAL GAP:
 *   The development_interests seat and the disaster_risk_scientists seat should compute radically different types from this same structural data. Development interests see a defunct prohibition, a heritage site to be managed ceremonially while the zone develops — they experience no constraint, only theatrical decoration. Disaster-risk scientists see a decayed early-warning system whose loss of behavioral force creates tail risk for future residents. The civic_authorities seat computes as the constraint's administrator (agenda_setter) who could re-operationalize it but chooses theatrical maintenance instead — they sit between the beneficiary (development) and the victim (future residents), and their directionality should reflect that incoherent position. The future residents compute as powerless victims at the moment of this story's authoring (they are not yet present); at the moment of disaster their seat will become the entire story's core. The engine's per-seat computation will expose these gaps; do not reconcile them in the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Development interests are beneficiaries (d near 0.0): they collect economic value from zone development under the cover of the stone's ceremonial maintenance. Civic authorities are agenda-setters (power institutional) who administer the stone's symbolic function and issue permits in the zone — they benefit from the theatrical arrangement (no conflict, cultural credit) and are powerful enough to maintain it (d ~ 0.3: beneficiary-adjacent, high power, mobile exit). Future residents are victims (d near 1.0): powerless, trapped (no knowledge of or consent to the risk), bearing the tail-risk cost. Disaster-risk scientists are observers (analytical power, analytical exit) — they document the decay but have no seat in the permits process. The key insight: this is NOT a constraint defending itself through high suppression because the rule is already dead as operative behavioral force. The suppression is LOW because no one is suppressing the prohibition's re-operationalization — development proceeds because the prohibition is successfully symbolized as historical artifact, not because it is being coercively maintained. This is the piton signature: inertial persistence through theatrical maintenance, not through active defense of extractive benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to transmit tsunami-hazard knowledge across generations and enforce safe settlement boundaries — is DEAD in this reading. The original Aneyoshi community solved it through embodied community rule. Contemporary authorities treat the problem as solved by modern building codes and scientific risk assessment, but the Aneyoshi case itself demonstrates the failure: the zone develops despite the historical warning, and future residents will lack the embodied knowledge that the original community possessed. The constraint's mandate has outlived its function. What persists is a DIFFERENT function: cultural vindication (the stone proves the community was wise and the memory is preserved). The theatrical maintenance serves this new function, not the original one. Mandatrophy is present: the constraint persists BECAUSE its original purpose is treated as solved, while the actual purpose (preventing settlement in the high-risk zone) fails silently. The piton classification captures this: the constraint looks like it does something (the stone sits in ceremonial maintenance) but what it actually does is enable extraction (development interests benefit from the zone's occupation while carrying zero catastrophic risk themselves).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_of_symbolic_death,
    'At what point did the prohibition transition from operative behavioral rule to ceremonial symbol? Was there a discrete moment of re-interpretation, or a gradual decay?',
    'Historical reconstruction of permit-issuance records, local government deliberations, and community practices around the stone across the interval. Interview surviving elders about when teaching the prohibition shifted from ''do not build here'' to ''remember that people did not build here''.',
    'If the transition was discrete and deliberate (a municipal reframing), the constraint is evidence of institutional capture of memory by development interests. If gradual, it suggests memory decay through generational distance — a failure of knowledge transmission rather than active suppression. The piton classification holds in both cases, but the mechanism differs (captured vs. decayed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_of_symbolic_death, empirical, 'Mechanism of decay from operational rule to ceremonial symbol').

omega_variable(
    knowledge_retention_bifurcation,
    'Do the original Aneyoshi community''s descendants (if present) retain the behavioral memory of the prohibition, even while contemporary authorities treat it as ceremonial?',
    'Ethnographic research with families claiming descent from Aneyoshi or with long residence in the area. Document what different cohorts know and teach about the stone''s meaning and the zone''s risk.',
    'If descendants retain behavioral memory, the constraint might be more resilient than the theatrical maintenance suggests — a shadow enforcement network might persist. If knowledge has completely decayed, the future residents below the mark will be the first generation with zero memory of the original rule, maximizing victim exposure. Alternatively, if descendants consciously reject the prohibition as outdated (belief shift), the mechanism is different from simple forgetting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_retention_bifurcation, empirical, 'Intergenerational transmission of behavioral knowledge vs. ceremonial knowledge').

omega_variable(
    development_regulatory_capture,
    'Are the civic authorities that permit development in the prohibited zone captured by development interests, or do they genuinely believe modern codes supersede the historical prohibition?',
    'Access to deliberation records, financial disclosure of decision-makers, comparative analysis of permit denials in the zone vs. outside. Document whether development interests provided funding, expertise, or political pressure that shaped the reframing of the stone as ceremonial.',
    'If capture is present, the constraint is snare-like (development extracts through coercion and suppression of the prohibition''s operative meaning). If genuine belief in regulatory supersession, the constraint is more piton-like (inertial, theatrical, but not deliberately extractive). The theater ratio (0.81) is high in both cases, but the role of development interests differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_regulatory_capture, empirical, 'Whether authorities treat the prohibition as genuinely obsolete or deliberately downgrade it').

omega_variable(
    disaster_return_probability,
    'What is the probability that a tsunami of the magnitude that triggered the original prohibition will return to the Aneyoshi zone within the next 50–100 years?',
    'Paleoseismic and tsunami-modeling literature. Historical tsunami records for the region. Geological evidence of recurrence intervals.',
    'If probability is high (>50% in the next 50 years), the victims (future residents) face substantial tail risk, and the extraction (development gains vs. catastrophic victim loss) is severe. If probability is low, the constraint might be better read as a false alarm (outdated precaution) rather than a suppressed warning. This feeds the mandatrophy analysis: if the disaster is genuinely low-probability, the prohibition''s decay might reflect rational updating rather than institutional failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disaster_return_probability, empirical, 'Geophysical basis for the original prohibition''s continued relevance').

omega_variable(
    reading_foreclosure_ambiguity,
    'Can the behavioral_competence_reading and the commemorative_husk_reading coexist within a single institutional framework, or does treating the prohibition as operative fundamentally conflict with treating it as purely ceremonial?',
    'Institutional design analysis: can a regulatory regime simultaneously enforce the prohibition as a land-use rule and honor it as a cultural monument, or does one function necessarily subsume the other? What would dual-function governance look like, and why does it not occur?',
    'If the readings coexist (different parties hold them simultaneously), the constraint is contested but stable — the reading_relations should be ''coexists_with''. If one reading logically forecloses the other within a single institutional seat, the relation should be ''forecloses''. The difference affects how the engine models institutional drift toward one reading or the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether the two readings are logically incompatible or merely held by different parties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(aney_tr_t0, observed).
narrative_ontology:measurement(aney_tr_t25, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(aney_tr_t25, observed).
narrative_ontology:measurement(aney_tr_t50, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(aney_tr_t50, observed).
narrative_ontology:measurement(aney_tr_t75, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 75, 0.58).
narrative_ontology:measurement_basis(aney_tr_t75, observed).
narrative_ontology:measurement(aney_tr_t100, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 100, 0.76).
narrative_ontology:measurement_basis(aney_tr_t100, observed).
narrative_ontology:measurement(aney_tr_t125, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 125, 0.81).
narrative_ontology:measurement_basis(aney_tr_t125, observed).
narrative_ontology:measurement(aney_tr_t150, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 150, 0.81).
narrative_ontology:measurement_basis(aney_tr_t150, projected).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(aney_be_t0, observed).
narrative_ontology:measurement(aney_be_t25, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement_basis(aney_be_t25, observed).
narrative_ontology:measurement(aney_be_t50, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement_basis(aney_be_t50, observed).
narrative_ontology:measurement(aney_be_t75, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement_basis(aney_be_t75, observed).
narrative_ontology:measurement(aney_be_t100, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 100, 0.72).
narrative_ontology:measurement_basis(aney_be_t100, observed).
narrative_ontology:measurement(aney_be_t125, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 125, 0.78).
narrative_ontology:measurement_basis(aney_be_t125, observed).
narrative_ontology:measurement(aney_be_t150, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 150, 0.78).
narrative_ontology:measurement_basis(aney_be_t150, projected).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(aney_su_t0, observed).
narrative_ontology:measurement(aney_su_t25, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(aney_su_t25, observed).
narrative_ontology:measurement(aney_su_t50, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(aney_su_t50, observed).
narrative_ontology:measurement(aney_su_t75, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 75, 0.45).
narrative_ontology:measurement_basis(aney_su_t75, observed).
narrative_ontology:measurement(aney_su_t100, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 100, 0.35).
narrative_ontology:measurement_basis(aney_su_t100, observed).
narrative_ontology:measurement(aney_su_t125, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 125, 0.32).
narrative_ontology:measurement_basis(aney_su_t125, observed).
narrative_ontology:measurement(aney_su_t150, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 150, 0.32).
narrative_ontology:measurement_basis(aney_su_t150, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.05).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The Aneyoshi land-use prohibition is a contested institutional kernel — two structurally distinct constraints arise from competing readings of the stone's meaning and force. The behavioral_competence_reading models the prohibition as a live, operationally enforced rule (ε ~ 0.08–0.15, low theater, high suppression). This reading (commemorative_husk) models the prohibition as a decayed ceremonial artifact whose behavioral force has evaporated (ε ~ 0.78, high theater, low suppression). The two readings are held by different institutional seats (disaster-risk scientists vs. development interests + civic authorities) and neither forecloses the other — they coexist as competing institutional narratives. Decomposing into two stories preserves the structural differences that would be lost if forced into one constraint with a 'measurement parameter' for observable choice. The high ε in this reading arises not from the prohibition's strength but from development interests' systematic capture of the zone's value under the cover of the stone's ceremonial maintenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
