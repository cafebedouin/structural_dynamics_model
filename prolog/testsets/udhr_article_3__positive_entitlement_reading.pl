% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 Positive Entitlement (State Provision of Material Conditions)
 *   domain: constitutional/human_rights
 *
 * SUMMARY:
 *   Article 3 of the Universal Declaration of Human Rights states: 'Everyone
 *   has the right to life, liberty and security of person.' This reading
 *   interprets that language as creating a positive obligation on states to
 *   provide the material conditions necessary for survival and security:
 *   welfare, healthcare, housing, protection from violence. This is a
 *   CONTESTED READING of a kernel (UDHR Article 3) with multiple defensible
 *   interpretations. The negative liberty reading sees Article 3 as a
 *   prohibition on state deprivation of life absent due process; the
 *   procedural hybrid reading centers due-process protections without
 *   resolving the substantive liberty/welfare dispute. This story
 *   instantiates the positive entitlement reading only, modeling its
 *   structural consequences. The claim/metric gap is intentional: the reading
 *   is asserted as Tangled Rope (coordination of welfare provision +
 *   asymmetric extraction from property holders) while metrics show
 *   substantial ongoing extraction (0.68 at interval end) and rising theater
 *   (performative administration masking the extraction gap between
 *   entitlement and actual provision).
 *
 * KEY AGENTS:
 *   - vulnerable_populations: Powerless beneficiaries; have no market income sufficient for material conditions; trapped within state jurisdiction; the structural beneficiary set whose claims justify the extraction.
 *   - state_welfare_apparatus: Institutional agenda-setter; translates entitlement into provision policy; absorbs legitimacy claims while operating a redistribution machine.
 *   - property_rights_holders: Powerful payers; face taxation justified by positive entitlement duty; can relocate assets but not easily exit jurisdiction; experience constraint on economic prerogatives.
 *   - expression_restricted_actors: Moderate-power payers; face speech restriction justified by need to protect vulnerable groups from harm; experience suppression justified by material security obligations.
 *   - liberal_constitutionalists: Organized excludes; would contest the positive interpretation; partially foreclosed from setting Article 3's meaning in adopting jurisdictions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.45).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 Positive Entitlement (State Provision of Material Conditions)").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '34925cf3-9d84-4281-ac66-5f1420200278').
narrative_ontology:cs_kernel_codification('34925cf3-9d84-4281-ac66-5f1420200278', fixed_text).
narrative_ontology:cs_authority_grounding('34925cf3-9d84-4281-ac66-5f1420200278', lineage).
narrative_ontology:cs_interpretation_layer_present('34925cf3-9d84-4281-ac66-5f1420200278').
narrative_ontology:cs_reading_relation('34925cf3-9d84-4281-ac66-5f1420200278', udhr_article_3__negative_liberty_reading, forecloses).
narrative_ontology:cs_reading_relation('34925cf3-9d84-4281-ac66-5f1420200278', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('34925cf3-9d84-4281-ac66-5f1420200278', foundational, material_conditions_prerequisite_to_rights).
narrative_ontology:cs_axiom_status(material_conditions_prerequisite_to_rights, holdable).
narrative_ontology:cs_axiom_grounding('34925cf3-9d84-4281-ac66-5f1420200278', material_conditions_prerequisite_to_rights, deontological).
narrative_ontology:cs_axiom('34925cf3-9d84-4281-ac66-5f1420200278', foundational, state_positive_duty_to_provide).
narrative_ontology:cs_axiom_status(state_positive_duty_to_provide, holdable).
narrative_ontology:cs_axiom_grounding('34925cf3-9d84-4281-ac66-5f1420200278', state_positive_duty_to_provide, deontological).
narrative_ontology:cs_reference_frame('34925cf3-9d84-4281-ac66-5f1420200278', material_security_as_human_right).
narrative_ontology:cs_drift_state('34925cf3-9d84-4281-ac66-5f1420200278', contemporary_welfare_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('34925cf3-9d84-4281-ac66-5f1420200278', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, economically_disadvantaged).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_rights_holders).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, high_income_earners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, expression_restricted_actors).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, social_contract_material_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups without sufficient market income to secure food, shelter, healthcare, or safety: the economically destitute, disabled, elderly without pension, chronically ill. Under this reading, they have a positive claim on state resources to meet baseline material conditions. Exit is impossible — they cannot leave the jurisdiction and simultaneously secure material conditions elsewhere without state provision.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% The administrative, legislative, and executive machinery tasked with translating the entitlement into operative welfare provision: tax collection, means-testing, benefit distribution, healthcare delivery, housing allocation. Sets the substantive meaning of Article 3 through budgetary and regulatory choices. The scope of provision, its adequacy, and its enforcement are all matters the apparatus adjudicates.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_welfare_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Individuals and entities with accumulated wealth, capital, real property subject to taxation to fund welfare provision. Under the positive entitlement reading, their property rights are subordinate to the state's duty to secure material conditions for the vulnerable. They bear the direct cost through taxation and regulatory constraint on economic activity. They can relocate assets or income across borders with difficulty; jurisdiction exit is impractical for many.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_rights_holders, payer,
    powerful, generational, constrained, national).

% Workers and professionals whose earnings are subject to progressive taxation justified by redistributive duty. They bear extraction through tax burden justified by the positive entitlement framework. Exit options include tax planning, relocation to lower-tax jurisdictions, or political organizing to shift the reading; full exit is costly but possible.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, high_income_earners, payer,
    powerful, biographical, constrained, national).

% Individuals and organizations restricted in speech, media, or assembly on grounds of hate speech prohibition or incitement doctrine enforced under the positive entitlement reading (protecting vulnerable groups from material harm through speech). They experience suppression of expression justified by the need to secure vulnerable populations' material conditions and safety. Exit via relocation or adopting restricted speech is possible; changing the legal framework requires coalition building.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, expression_restricted_actors, payer,
    moderate, biographical, constrained, national).

% Constitutional scholars, advocates, and political movements committed to negative liberty framing of rights (freedom from state interference). They would argue Article 3 protects only non-interference and procedural justice, not positive entitlement. They are partially excluded from adjudicating the meaning of Article 3 in jurisdictions that have adopted the positive entitlement reading, though they retain voice through litigation and academic influence.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, liberal_constitutionalists, excluded,
    organized, generational, constrained, global).

% UN treaty monitoring bodies, regional human rights courts, and international NGOs that interpret and enforce UDHR Article 3. They observe and assess whether state parties are meeting the positive entitlement obligations: whether material conditions are actually provided, adequacy of provision, non-discrimination in access. Their interpretations influence state behavior through soft law and reputational pressure.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Political coalitions and economic interests opposed to the material cost of positive entitlement provision. They are excluded from setting the binding interpretation of Article 3 in jurisdictions that have committed to the positive reading, though they retain voice through elections and legislative opposition. Their exclusion is structural: a commitment to positive entitlement necessarily restricts the policy space they can occupy.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, fiscal_conservatives, excluded,
    powerful, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, state_welfare_apparatus).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a collective commitment to material security as a human right rather than a commodity: pools resources through taxation and state apparatus to ensure all members meet baseline material conditions (food, shelter, healthcare, safety). Solves the collective-action problem of moral hazard in provision (free-rider problem if provision were voluntary) and the information problem of identifying who genuinely cannot meet conditions via market means.
% TRANSFER_FUNCTION: Transfers wealth from property holders and high earners to vulnerable populations unable to secure material conditions through market participation. The mechanism is progressive taxation, regulatory constraint on economic activity, and mandatory state provision of welfare services. The moral justification is that material security is a prerequisite for meaningful exercise of other rights.
% ABSENT_VOICES: Jurisdictions that have not adopted the positive entitlement reading (those operating under negative liberty or procedural hybrid frameworks) are absent from this reading's operative community. Liberal constitutionalists within adopting states are partially excluded — they retain voice through courts and scholarship but do not set the binding meaning of Article 3. Property-rights fundamentalists are structurally excluded because the reading subordinates property to material security.
% DISAPPEARANCE_RATIONALE: If the positive entitlement interpretation of Article 3 vanished overnight, the welfare apparatus would contract sharply, vulnerable populations would lose claim to state provision of material conditions, property taxation would shift downward, and distribution would revert to market mechanisms. A significant fraction of the global welfare state (healthcare provision, housing assistance, pension systems, social insurance) in countries that have adopted this reading exists because Article 3 is interpreted this way; removal would require radical institutional reorganization and would predictably harm vulnerable populations.
% FOUNDING_PROBLEM: The foundational problem this reading addresses: natural liberty alone (freedom from state interference) leaves people without material conditions for survival and security; market mechanisms are insufficient and unreliable for ensuring all persons meet baseline subsistence and safety. The reading asserts that human rights commitments are hollow if they do not include a right to the material conditions without which other rights are meaningless.
% FOUNDING_PROBLEM_CORROBORATION: Advocates of the positive entitlement reading attest the founding problem remains live: poverty, inadequate healthcare access, housing insecurity persist even in wealthy jurisdictions. International human rights bodies corroborate that material conditions are unmet in many countries. However, negative liberty advocates dispute both the diagnosis (claiming market and civil-society provision are sufficient and preferable) and the causal premise (that state provision is an effective solution). Empirical data on poverty rates and healthcare outcomes are cited by both readings but interpreted differently.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at end) reflects the magnitude of wealth transfer justified by positive entitlement interpretation plus the scope of regulatory constraint on property and expression. The measurement series shows steady rise from 0.45 to 0.68, indicating accumulation: as welfare apparatus expands and fiscal demands grow, effective extraction intensifies. Theater (0.38) is moderate — welfare bureaucracy has real function (assessment, delivery, prevention of fraud) but a growing share of administrative effort is devoted to managing the constraint itself (compliance monitoring, appeal systems, rationing mechanisms) rather than provision. Suppression (0.45) is moderate-low because the constraint operates through law and administration rather than violence; however, it does require active suppression of alternative property regimes and speech constraints justified by protective duty. The reading is Tangled Rope because: (1) genuine coordination function exists (collective provision of material security is a real coordination problem), (2) asymmetric extraction occurs (property holders and high earners bear costs disproportionate to benefits), (3) active enforcement is required (tax administration, welfare means-testing, speech regulation all require state apparatus to maintain the distribution).
 *
 * PERSPECTIVAL GAP:
 *   From the vulnerable-populations seat, the constraint is experienced as life-enabling: material conditions are provided that would be unavailable via market alone. Extraction is not salient because the alternative is deprivation. From the property-holder seat, extraction is highly salient: wealth is transferred via taxation justified by Article 3's positive reading, and property prerogatives are constrained. The state-apparatus seat sits between: it administers provision and justifies extraction, but also bears legitimacy risk if provision is inadequate (theater rises as gap between entitlement and actual provision grows). The liberal-constitutionalist excluded seat would experience the constraint as unjust foreclosure: Article 3 is read to prohibit rather than mandate state action, making the positive reading's extraction unjustified.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations (powerless, trapped): low d (0.1–0.15) because they are structural beneficiaries; the constraint subordinates all else to their material security. Property holders (powerful, constrained exit): high d (0.85–0.90) because extraction is their primary experience; they bear the cost and cannot easily exit the jurisdiction. High-income earners (powerful, constrained exit): high d (0.75–0.80) because progressive taxation is their experience, though they have some exit options via tax planning or relocation. Expression-restricted actors (moderate power, constrained exit): moderate d (0.55–0.65) because they experience suppression but the suppression is justified by a welfare reading that claims their speech would harm vulnerable populations. No directionality overrides are needed because the automatic derivation from beneficiary/victim + exit captures the true structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is live and uncontested across readings (material insecurity exists; all readings acknowledge this). The core contest is WHETHER Article 3 creates a positive duty to provide material conditions or merely prohibits state deprivation. The positive entitlement reading asserts the former; if that reading is correct, no mandatrophy exists — the founding problem persists and the constraint remains justified. However, there is a secondary mandatrophy risk: if the welfare apparatus becomes performative (theater rising, actual provision stagnating), the constraint might persist as routine without solving the founding problem it claims to address. The measurement series shows theater rising to 0.38 by end-of-interval with extractiveness also rising (0.68), suggesting possible drift toward a performative state where the constraint collects redistribution without proportionally improving material conditions — a Piton-adjacent trajectory. The challenge is distinguishing genuine expansion (more material provision) from administrative theater (more bureaucracy to manage the same provision). This is unresolved and should trigger an omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positive_obligation_vs_prohibition,
    'Does Article 3''s language create a positive obligation on states to provide material conditions, or only a negative obligation to refrain from deprivation?',
    'Textual analysis, drafting history of the UDHR, and comparative constitutional jurisprudence. Courts in different jurisdictions have resolved this differently; no universally binding authority exists.',
    'If resolved toward positive obligation, this constraint (Tangled Rope with substantial extraction) is correctly classified. If resolved toward prohibition-only, the constraint should reclassify toward Rope (coordination without asymmetric extraction) or even Mountain (fundamental principle without redistribution). The reading fundamentally determines ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(positive_obligation_vs_prohibition, conceptual, 'Whether Article 3 mandates state provision or merely restricts state deprivation.').

omega_variable(
    material_sufficiency_gap,
    'What is the gap between Article 3''s entitlement (as asserted by this reading) and the actual material provision states deliver in practice?',
    'Comparative measurement of poverty rates, healthcare access, housing adequacy, mortality, and morbidity across jurisdictions adopting the positive reading vs. others. Rising gap would indicate theater; stable or closing gap would indicate genuine coordination function.',
    'If gap is large and widening, theater_ratio underestimates the constraint''s performative character (Piton candidate). If gap is stable or closing, the constraint is genuinely coordinating material provision and is correctly classified as Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(material_sufficiency_gap, empirical, 'Whether the positive entitlement is being operationally honored or merely rhetorically maintained.').

omega_variable(
    welfare_extraction_vs_coordination,
    'Is the extraction (0.68) primarily a cost of coordination (administrative overhead, incentive structures necessary for provision), or primarily a transfer to political/administrative classes managing the apparatus?',
    'Detailed audit of welfare bureaucracy spending, comparing administrative overhead to direct provision; analysis of state-apparatus jobs created relative to beneficiary-population needs met.',
    'If extraction is mainly coordination cost, the constraint is correctly Tangled Rope. If extraction is primarily transfer to administrative classes while direct provision stagnates, the constraint is Snare with a Rope cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_extraction_vs_coordination, empirical, 'Whether measured extraction serves the stated coordination function or has become independent.').

omega_variable(
    reading_suppression_mechanism,
    'Is the suppression of expression and property rights (suppression_requirement = 0.45) structural (necessary to protect material security claims from contestation) or internalized (vulnerable populations and beneficiaries self-censor and abandon property claims out of identity fusion with entitlement status)?',
    'Post-exit analysis: if individuals who exit jurisdictions adopting the positive reading experience sustained self-suppression, the mechanism is internalized; if suppression is jurisdiction-specific, it is primarily structural.',
    'If structural, the suppression is an enforced feature of the constraint; if internalized, the constraint carries higher effective suppression than the 0.45 metric suggests, and the constraint''s persistence is more deeply embedded in identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_suppression_mechanism, empirical, 'Whether suppression of alternative readings is external enforcement or internalized identity.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the positive entitlement reading''s assertion of material-security obligation logically foreclose the negative liberty reading, or can both readings coexist as live positions within a single jurisdiction?',
    'Logical analysis: a court could hold that Article 3 requires positive provision AND that property rights are fundamental — but the resource constraint creates conflict. If the readings can be held simultaneously without contradiction, they coexist; if the resource constraint forces a choice, one forecloses the other.',
    'If foreclosure: the relation to the negative reading is ''forecloses''. If coexistence: the relation is ''coexists_with''. This determines the cs_structure.reading_relations value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the positive and negative readings are logically exclusive or both sustainable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__positive_entitlement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(udhr_tr_t0, projected).
narrative_ontology:measurement(udhr_tr_t10, udhr_article_3__positive_entitlement_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(udhr_tr_t10, projected).
narrative_ontology:measurement(udhr_tr_t20, udhr_article_3__positive_entitlement_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement_basis(udhr_tr_t20, observed).
narrative_ontology:measurement(udhr_tr_t35, udhr_article_3__positive_entitlement_reading, theater_ratio, 35, 0.38).
narrative_ontology:measurement_basis(udhr_tr_t35, observed).
narrative_ontology:measurement(udhr_tr_t50, udhr_article_3__positive_entitlement_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement_basis(udhr_tr_t50, observed).
narrative_ontology:measurement(udhr_tr_t75, udhr_article_3__positive_entitlement_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement_basis(udhr_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__positive_entitlement_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(udhr_be_t0, projected).
narrative_ontology:measurement(udhr_be_t10, udhr_article_3__positive_entitlement_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(udhr_be_t10, projected).
narrative_ontology:measurement(udhr_be_t20, udhr_article_3__positive_entitlement_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(udhr_be_t20, observed).
narrative_ontology:measurement(udhr_be_t35, udhr_article_3__positive_entitlement_reading, base_extractiveness, 35, 0.64).
narrative_ontology:measurement_basis(udhr_be_t35, observed).
narrative_ontology:measurement(udhr_be_t50, udhr_article_3__positive_entitlement_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement_basis(udhr_be_t50, observed).
narrative_ontology:measurement(udhr_be_t75, udhr_article_3__positive_entitlement_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement_basis(udhr_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__positive_entitlement_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(udhr_su_t0, projected).
narrative_ontology:measurement(udhr_su_t10, udhr_article_3__positive_entitlement_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(udhr_su_t10, projected).
narrative_ontology:measurement(udhr_su_t20, udhr_article_3__positive_entitlement_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(udhr_su_t20, observed).
narrative_ontology:measurement(udhr_su_t35, udhr_article_3__positive_entitlement_reading, suppression_requirement, 35, 0.45).
narrative_ontology:measurement_basis(udhr_su_t35, observed).
narrative_ontology:measurement(udhr_su_t50, udhr_article_3__positive_entitlement_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement_basis(udhr_su_t50, observed).
narrative_ontology:measurement(udhr_su_t75, udhr_article_3__positive_entitlement_reading, suppression_requirement, 75, 0.45).
narrative_ontology:measurement_basis(udhr_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(udhr_article_3__positive_entitlement_reading, 0.18).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% Article 3 of the UDHR is the kernel. Three structurally distinct readings generate three separate constraints with different ε values, beneficiary/victim structures, and classifications. The positive_entitlement_reading (this story) asserts Article 3 mandates state provision of material conditions, generating substantial extraction from property holders (ε=0.68, Tangled Rope). The negative_liberty_reading interprets Article 3 as a prohibition on state deprivation without positive obligation, yielding lower extraction and a Rope or Mountain classification. The procedural_hybrid_reading centers due-process protections (habeas corpus, torture prohibition) and defers the substantive liberty/welfare dispute, yielding a different extraction profile and stakeholder structure. Each reading has its own beneficiary/victim set, enforcement requirements, and measurable consequences. All three are live interpretations held by different state parties. Network linkage enables analysis of how the three readings contaminate each other's legitimacy and what empirical evidence would privilege one over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
