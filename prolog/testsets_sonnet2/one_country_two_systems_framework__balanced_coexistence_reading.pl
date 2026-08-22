% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems — Balanced Coexistence Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the balanced-coexistence reading of the One
 *   Country, Two Systems kernel: neither sovereignty nor autonomy is treated
 *   as absolute; the boundary between them is understood as continuously
 *   negotiated through political accommodation (NPCSC interpretations,
 *   periodic crises like 2003's Article 23 withdrawal, 2014's Umbrella
 *   Movement, 2019's extradition-bill protests, 2020's National Security Law)
 *   rather than settled by a single legal doctrine. Under this reading, the
 *   framework functions as a tangled rope: it genuinely coordinates the
 *   incorporation of a structurally different economic and legal system into
 *   a unitary sovereign state (the coordination function), while
 *   simultaneously enabling asymmetric extraction from residents and
 *   activists who bear the cost whenever a boundary dispute resolves toward
 *   sovereignty (the extraction function), enforced through security
 *   legislation, judicial reinterpretation, and administrative discretion.
 *   The two sibling readings — sovereignty_primacy (which treats autonomy as
 *   fully revocable delegation) and autonomy_primacy (which treats autonomy
 *   as treaty-guaranteed and internationally enforceable) — are NOT
 *   represented in this file; they are separate constraints with their own ε
 *   and stakeholder structures.
 *
 * KEY AGENTS:
 *   - prc_central_government: sets ultimate boundary through NPCSC interpretation, benefits from successful integration narrative
 *   - hong_kong_government: administers the negotiated boundary locally, absorbs enforcement cost
 *   - hong_kong_judiciary: performs the case-by-case negotiation of how far judicial independence extends
 *   - hong_kong_business_elite / international_investors: retain exit leverage that shapes negotiation incentives
 *   - hong_kong_prodemocracy_activists / hong_kong_working_residents: bear the cost when the boundary shifts toward sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.52).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.48).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems — Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '2d20acd5-f387-4529-825e-f5556ccd2646').
narrative_ontology:cs_kernel_codification('2d20acd5-f387-4529-825e-f5556ccd2646', formalized).
narrative_ontology:cs_authority_grounding('2d20acd5-f387-4529-825e-f5556ccd2646', extraction).
narrative_ontology:cs_interpretation_layer_present('2d20acd5-f387-4529-825e-f5556ccd2646').
narrative_ontology:cs_reading_relation('2d20acd5-f387-4529-825e-f5556ccd2646', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d20acd5-f387-4529-825e-f5556ccd2646', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('2d20acd5-f387-4529-825e-f5556ccd2646', foundational, boundary_is_continuously_negotiated_not_fixed).
narrative_ontology:cs_axiom_status(boundary_is_continuously_negotiated_not_fixed, holdable).
narrative_ontology:cs_axiom_grounding('2d20acd5-f387-4529-825e-f5556ccd2646', boundary_is_continuously_negotiated_not_fixed, conventional).
narrative_ontology:cs_axiom('2d20acd5-f387-4529-825e-f5556ccd2646', secondary, civil_society_retains_genuine_bargaining_leverage).
narrative_ontology:cs_axiom_status(civil_society_retains_genuine_bargaining_leverage, holdable).
narrative_ontology:cs_axiom_grounding('2d20acd5-f387-4529-825e-f5556ccd2646', civil_society_retains_genuine_bargaining_leverage, empirically_contingent).
narrative_ontology:cs_reference_frame('2d20acd5-f387-4529-825e-f5556ccd2646', joint_declaration_transitional_accommodation).
narrative_ontology:cs_drift_state('2d20acd5-f387-4529-825e-f5556ccd2646', post_2020_national_security_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2d20acd5-f387-4529-825e-f5556ccd2646', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elite).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, international_investors).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_prodemocracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_working_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_working_residents).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate sovereign authority under the Basic Law and asserts final interpretive power (through the NPC Standing Committee) whenever a boundary dispute arises. Negotiates the scope of Hong Kong's autonomy case by case rather than through a settled legal doctrine, calibrating concessions to preserve the appearance of 'two systems' while keeping national security and appointment powers as an unqualified reserve. Bears reputational and economic costs internationally if the accommodation model visibly collapses, so retains some incentive to negotiate rather than simply override.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, beneficiary).

% Administers the local legal and administrative system, mediates between Beijing's directives and local expectations, and absorbs the political cost of enforcing centrally set boundaries it did not fully choose. Its legitimacy depends on being seen as functionally autonomous even as its discretion over security and appointments narrows.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government, payer).

% Continues to apply common-law procedure and precedent within Hong Kong courts, but operates under the shadow of NPCSC interpretive override on constitutional questions. Judges negotiate case-by-case how far judicial independence extends before triggering a reinterpretation; this negotiation IS the functional division of powers the reading describes.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judiciary, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judiciary, observer).

% Benefits from the common-law commercial system, separate currency, and international financial access that the 'two systems' arrangement preserves, while accommodating political constraints imposed from the mainland side. Retains capital mobility and international legal recourse that gives it genuine bargaining leverage in the ongoing negotiation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elite, beneficiary,
    powerful, biographical, mobile, global).

% Uses Hong Kong as a gateway combining mainland market access with common-law contract enforcement and free capital flow. Their continued willingness to operate through Hong Kong is itself a bargaining chip that both Beijing and the Hong Kong government must weigh when negotiating boundary disputes.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Pushes for the autonomy side of the negotiation to hold firm — electoral competitiveness, protest rights, press freedom — and bears the direct cost when sovereignty concerns override those claims (arrests, disqualifications, exile). Has some leverage through international attention and diaspora networks but cannot compel outcomes; the accommodation reading treats their losses as the price of a negotiated boundary rather than a clean victory for either side.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_prodemocracy_activists, payer,
    moderate, biographical, trapped, national).

% Lives inside whatever balance the negotiation currently produces — benefiting from continued economic openness and rule-of-law commercial dispute resolution, but absorbing the social and political cost when the boundary shifts toward sovereignty (security law prosecutions, narrowing civic space) without having a direct seat at the negotiating table.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_working_residents, payer,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_working_residents, beneficiary).

% Signatories and observers to the Sino-British Joint Declaration and related instruments assert an interest in how the boundary is negotiated but have no enforcement mechanism beyond diplomatic pressure, sanctions, and market signaling — tools that shape the negotiation's costs without controlling its outcome.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_governments_and_treaty_parties, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a single sovereign state to incorporate a territory with a structurally different legal, economic, and civic system without forcing immediate convergence — preserving Hong Kong's role as a functioning international financial and legal gateway while affirming PRC sovereignty, avoiding the destabilization either full absorption or full independence would cause.
% TRANSFER_FUNCTION: Moves political and civic space from Hong Kong's autonomous institutions toward central sovereign prerogative whenever a boundary dispute is resolved through reinterpretation rather than settled precedent; moves economic and reputational benefits toward business and investor classes who can exit or arbitrage across both systems, and away from residents and activists who cannot.
% ABSENT_VOICES: Ordinary Hong Kong residents without international mobility, and grassroots civic organizations dissolved or exiled since 2020, are not present in the negotiation between Beijing and Hong Kong's governing and business elites; their preferences are represented, if at all, through proxies (activists in exile, foreign legislatures) whose leverage is indirect and diplomatic rather than structural.
% DISAPPEARANCE_RATIONALE: If the framework's negotiated character disappeared overnight and either sovereignty or autonomy became absolute, business elites and investors argue the financial gateway function would collapse (supporting continued negotiation); Beijing argues full integration would resolve chronic instability at acceptable cost; activists argue autonomy was already effectively extinguished and its formal disappearance would change little for those already living under the security-law regime. The verdict differs by seat, which is itself evidence that the 'balanced negotiation' description, not a settled endpoint, is the operative reality.
% FOUNDING_PROBLEM: How to reunify a territory with a distinct legal, economic, and civic system into a socialist unitary state without triggering capital flight, international backlash, or a legitimacy crisis over broken pre-handover commitments.
% FOUNDING_PROBLEM_CORROBORATION: PRC officials and Hong Kong government spokespeople attest the framework continues to function as designed, citing continued financial-hub status. International legal scholars, former Hong Kong judges who have resigned from the bench citing eroded independence, and foreign government assessments (e.g. periodic US and UK statements on the Joint Declaration) attest from outside the beneficiary set that the negotiated balance has shifted substantially toward sovereignty since 2020, without formally declaring the framework dead.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, contested).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a medium level (0.52) reflecting the reading's own claim that neither party is fully dominant — this is lower than a pure sovereignty-primacy override would produce and higher than a pure treaty-guarantee reading would produce, because the balanced-coexistence reading itself asserts that costs are genuinely shared and periodically renegotiated rather than one-sidedly extracted. Suppression sits at a comparable medium level (0.48) because active enforcement (security law prosecutions, disqualifications, media closures) is real but is authored, under this reading, as a negotiating instrument rather than an unconstrained one — business and investor exit options impose real costs on Beijing and the HK government for overreach, which is why the theater_ratio and suppression_requirement measurements show a peak around 2020 and a partial retreat afterward rather than a monotonic ratchet. Accessibility_collapse (0.45) and resistance (0.6) are authored at moderate levels: alternatives to the negotiated arrangement (full independence, full integration) remain conceivable and are actively argued by different constituencies, and resistance from activists, the judiciary's residual independence, and international leverage is real, not merely nominal — consistent with a contested tangled rope rather than a settled mountain or naked snare.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute meaningfully different seat classifications: from prc_central_government's institutional/arbitrage seat, the arrangement looks like successful, low-cost coordination management; from hong_kong_prodemocracy_activists' powerless/trapped seat, the same structure computes as much closer to extraction, because their exit options and power level amplify effective extraction under the engine's directionality formula. This divergence is exactly what the balanced-coexistence reading predicts and what a single averaged ε would erase — which is why this reading is authored as its own constraint rather than folded into either sibling reading.
 *
 * DIRECTIONALITY LOGIC:
 *   prc_central_government and hong_kong_business_elite/international_investors are declared beneficiaries because they collect the arrangement's benefits (sovereign consolidation; financial gateway access) without bearing its costs directly, and hold arbitrage/mobile exit options that the engine should read as damping their effective extraction toward the subsidy end. hong_kong_prodemocracy_activists and hong_kong_working_residents are declared victims because they bear the costs of boundary shifts (prosecutions, narrowing civic space, economic precarity) with trapped or merely constrained exit, which the engine should read as amplifying their effective extraction toward the full-target end. hong_kong_government and hong_kong_judiciary are agenda_setters who also partly pay the political cost of enforcing a boundary set partly above them — a genuinely dual-positioned seat, hence the secondary_role declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reunifying a legally and economically distinct territory without destabilizing it) is authored as contested rather than flatly dead or live: business and investor beneficiaries attest the arrangement still functions as designed; independent observers (resigned judges, foreign governments) attest the negotiated balance has shifted substantially toward sovereignty since 2020 without the framework being formally abandoned. This mismatch — status=contested against a contested disappearance_verdict — is precisely the signal the classification should surface, rather than either declaring the framework a fully resolved success or a fully expired sham. Classifying it as tangled_rope (not scaffold, not snare) reflects that both a genuine coordination function and asymmetric extraction persist simultaneously and require active enforcement to hold, which the schema's tangled_rope gate is designed to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negotiation_vs_ratchet_ambiguity,
    'Is the boundary between sovereignty and autonomy genuinely renegotiated in both directions over time (as this reading claims), or does each crisis produce a one-way ratchet toward sovereignty that the ''negotiation'' framing obscures?',
    'Track whether any post-2020 boundary dispute has resolved in favor of expanded (not merely preserved) Hong Kong autonomy; a purely one-directional record across multiple crisis cycles would undermine the balanced-coexistence characterization and support reclassifying the arrangement closer to the sovereignty_primacy reading.',
    'If the ratchet-only pattern is confirmed over a longer interval, this reading''s own coordination claim weakens and the constraint drifts toward snare; if genuine two-directional renegotiation is observed (e.g. eased enforcement, restored electoral competitiveness), the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_vs_ratchet_ambiguity, empirical, 'Whether the negotiation is genuinely bidirectional or a disguised one-way sovereignty ratchet.').

omega_variable(
    civil_society_leverage_durability,
    'Does civil society''s bargaining leverage (economic exit threats, international attention, diaspora advocacy) actually constrain Beijing''s and the Hong Kong government''s choices in practice, or is it primarily symbolic, mattering only insofar as it does not threaten core sovereign prerogatives?',
    'Compare episodes where international/business pressure preceded a policy retreat (e.g. extradition bill withdrawal in 2019) against episodes where equivalent pressure had no visible effect (e.g. National Security Law implementation in 2020) to assess whether leverage operates conditionally rather than reliably.',
    'If leverage is shown to operate only when it does not touch core sovereignty/security questions, the ''substantive negotiation'' framing central to this reading is significantly narrower than authored, pushing the effective classification closer to sovereignty_primacy for the domains that matter most.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_leverage_durability, empirical, 'Whether civil society and business leverage meaningfully constrains outcomes or only operates at the margins.').

omega_variable(
    cs_framing_kernel_vs_institution,
    'Should the commitment-system kernel be located in the Basic Law text itself (a fixed_text framing) or in the higher-order legitimacy claim that PRC sovereign authority is compatible with genuine local autonomy (a formalized doctrinal framing)? These produce different reading_relations: a fixed_text framing emphasizes textual interpretation disputes (NPCSC vs. Hong Kong courts), while a formalized-doctrine framing emphasizes the legitimacy narrative of ''one country, two systems'' as a governing philosophy that outlives any specific interpretation dispute.',
    'Examine whether NPCSC interpretations are justified primarily by textual argument (fixed_text signal) or by appeal to the doctrine''s overall legitimacy and stability (formalized signal) in official statements accompanying major interpretive actions.',
    'Under the fixed_text framing, drift is absorbed as interpretation and the kernel appears more stable; under the formalized-doctrine framing, drift is more visible as doctrinal erosion, and axiom_overriding becomes a more salient risk to the reading''s coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_institution, conceptual, 'Whether the kernel is best modeled as the Basic Law text or the higher-order legitimacy doctrine layered above it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 1997, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 1997, 0.15).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2014, 0.28).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2019, 0.32).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(one__tr_t2023, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2023, 0.42).
narrative_ontology:measurement(one__tr_t2027, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2027, 0.4).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 1997, 0.32).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2014, 0.45).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(one__be_t2023, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2023, 0.55).
narrative_ontology:measurement(one__be_t2027, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2027, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 1997, 0.25).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2003, 0.32).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2019, 0.48).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(one__su_t2023, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement(one__su_t2027, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2027, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'One Country, Two Systems' claim per the ε-invariance principle. sovereignty_primacy_reading treats autonomy as fully revocable delegation (lower ε for Beijing-aligned seats, near-zero coordination friction from the sovereign's perspective); autonomy_primacy_reading treats autonomy as treaty-guaranteed and internationally enforceable (would assert much lower ε for Hong Kong civil seats if the guarantee held, and correspondingly higher ε once the guarantee is shown unenforceable). This balanced_coexistence_reading sits structurally between them, authoring a medium ε (0.52) that reflects genuine bidirectional bargaining rather than either pole. All three are linked here and should each link back to their siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
