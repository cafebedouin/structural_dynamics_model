% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Sovereignty-Primary Reading of Territorial Border Authority
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This story instantiates ONLY the sovereignty-primary reading of the
 *   contested border normative-status kernel: the claim that territorial
 *   boundaries are legitimate instruments of collective self-determination,
 *   grounding a state's foundational (not merely instrumental or conditional)
 *   authority to exclude non-members. Under this reading, excluded migrants,
 *   denied asylum seekers, and stateless persons are structural victims —
 *   their costs are not weighed against the exclusion power in a
 *   proportionality test, because the exclusion power is treated as prior and
 *   foundational rather than conditional. Border enforcement is read as a
 *   legitimate exercise of a real function (democratic self-governance
 *   requires a bounded demos), not merely as extraction dressed as
 *   self-determination. Displacement of border communities is treated within
 *   this reading as an externality of legitimate sovereign action, not an
 *   independent harm requiring separate justification — this is the key
 *   structural delta from the qualified_sovereignty and freedom_primary
 *   siblings, which are NOT part of this story and are generated as separate
 *   constraint files linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - citizen_polity: beneficiary — retains self-governance and resource control
 *   - border_enforcement_apparatus: agenda_setter — administers and enforces the exclusion power, with independent institutional incentives
 *   - excluded_migrants / asylum_seekers_denied_entry / stateless_persons: payers — bear the full cost of exclusion with no legitimacy-contesting channel
 *   - displaced_border_communities: excluded — costs treated as externality, not part of the proportionality conversation this reading does not run
 *   - international_legal_scholars: observer — analyzes the doctrinal architecture across sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.58).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.72).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Sovereignty-Primary Reading of Territorial Border Authority").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, '343ec54f-599c-4f4a-9254-c83deb303da5').
narrative_ontology:cs_kernel_codification('343ec54f-599c-4f4a-9254-c83deb303da5', distributed).
narrative_ontology:cs_authority_grounding('343ec54f-599c-4f4a-9254-c83deb303da5', distributed).
narrative_ontology:cs_reading_relation('343ec54f-599c-4f4a-9254-c83deb303da5', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('343ec54f-599c-4f4a-9254-c83deb303da5', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('343ec54f-599c-4f4a-9254-c83deb303da5', foundational, exclusion_authority_is_foundational_not_conditional).
narrative_ontology:cs_axiom_status(exclusion_authority_is_foundational_not_conditional, holdable).
narrative_ontology:cs_axiom_grounding('343ec54f-599c-4f4a-9254-c83deb303da5', exclusion_authority_is_foundational_not_conditional, deontological).
narrative_ontology:cs_axiom('343ec54f-599c-4f4a-9254-c83deb303da5', foundational, collective_self_determination_grounds_territorial_membership_control).
narrative_ontology:cs_axiom_status(collective_self_determination_grounds_territorial_membership_control, holdable).
narrative_ontology:cs_axiom_grounding('343ec54f-599c-4f4a-9254-c83deb303da5', collective_self_determination_grounds_territorial_membership_control, deontological).
narrative_ontology:cs_axiom('343ec54f-599c-4f4a-9254-c83deb303da5', secondary, displacement_externalities_require_no_independent_justification).
narrative_ontology:cs_axiom_status(displacement_externalities_require_no_independent_justification, holdable).
narrative_ontology:cs_axiom_grounding('343ec54f-599c-4f4a-9254-c83deb303da5', displacement_externalities_require_no_independent_justification, conventional).
narrative_ontology:cs_reference_frame('343ec54f-599c-4f4a-9254-c83deb303da5', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('343ec54f-599c-4f4a-9254-c83deb303da5', post_1951_refugee_convention_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('343ec54f-599c-4f4a-9254-c83deb303da5', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_polity).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, national_labor_incumbents).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, border_enforcement_apparatus).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers_denied_entry).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, stateless_persons).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, collective_self_determination_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, state_territorial_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of the bounded political community whose collective self-determination the border is said to secure: they retain control over membership, resource distribution, cultural continuity, and democratic decision-making within the territory. They can exit individually via emigration but collectively hold and exercise the exclusion power through the state.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_polity, beneficiary,
    organized, generational, mobile, national).

% Domestic workers whose wage and employment position benefits from restricted labor market entry by non-citizens. They support border enforcement as protection against wage competition, though this benefit is contested empirically and unevenly distributed across sectors.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, national_labor_incumbents, beneficiary,
    moderate, biographical, constrained, national).

% The immigration agencies, border patrol forces, and detention infrastructure that administer and physically enforce the exclusion power. They set operational policy within legislative mandates, control who is detained or deported, and their institutional budget and mandate grow with enforcement intensity — creating an incentive structure independent of the underlying sovereignty claim.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, border_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Persons seeking to cross the border for economic opportunity, family reunification, or safety who are turned back, detained, or deported. They bear the full cost of the exclusion — foreclosed livelihoods, family separation, danger in transit or at origin — with no channel to contest the legitimacy of the boundary itself, only case-by-case administrative appeals within a system that presumes exclusion as default.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% People fleeing persecution or violence whose claims are processed (or blocked from processing) at the border under a legal architecture that treats sovereignty as prior to protection obligations. Under this reading, the state's discretion to exclude is treated as foundational, so asylum processing is a state-granted accommodation rather than a claim the border itself must yield to.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers_denied_entry, payer,
    powerless, immediate, trapped, global).

% Persons with no recognized membership in any bounded polity. The sovereignty-primary framework, by grounding legitimate exclusion in the exercise of a state's self-determination, offers stateless persons no home jurisdiction whose self-determination could ever include them — they are structurally outside every polity's collective 'self' that borders protect.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, stateless_persons, payer,
    powerless, civilizational, trapped, global).

% Populations living along militarized or hardened border zones whose land, mobility, and cross-border kinship or trade networks are disrupted by enforcement infrastructure. Under this reading their displacement is treated as an externality of legitimate sovereign action rather than a cost requiring independent justification — they are not part of the conversation about whether the border's costs are proportionate.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, displaced_border_communities, excluded,
    powerless, generational, constrained, local).

% Scholars and jurists who study the doctrinal architecture of sovereignty claims, comparing this reading's foundational-authority premise against qualified and freedom-primary alternatives, without themselves being subject to the border's enforcement.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, diffuse).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A bounded political community coordinates collective self-governance — deciding who shares in its resources, laws, and democratic voice — which requires some mechanism for defining membership and its territorial correlate.
% TRANSFER_FUNCTION: The arrangement moves the burden of exclusion from the citizen polity (which retains undiminished access to territory, labor markets, and resources) onto non-members seeking entry, who bear displacement, foreclosed opportunity, danger, and in the asylum context, denial of protection, entirely without the possibility of appealing the boundary's own legitimacy.
% ABSENT_VOICES: Excluded migrants, asylum seekers, and stateless persons have no seat in the polity that decides the border's terms — that is structurally definitional to the arrangement (only members deliberate on membership rules). Displaced border communities are nominally citizens but their localized costs are treated as incidental to the national self-determination claim, not as a cost requiring separate justification.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primary framework's foundational-authority claim disappeared overnight and no successor legitimacy claim replaced it, the legal basis for exclusion at the state's sole discretion would collapse: enforcement agencies would lose their normative warrant (though not necessarily their coercive capacity), migration flows would reorganize substantially, national labor markets would face rapid entry pressure, and international law would need an entirely different foundational grammar for border legitimacy (likely shifting toward one of the sibling readings).
% FOUNDING_PROBLEM: Bounded political communities need some way to determine who participates in collective self-governance, who shares finite public goods, and who bears the obligations of citizenship — a problem inherent to any scheme of democratic self-rule at less than global scale.
% FOUNDING_PROBLEM_CORROBORATION: States and their citizen polities attest the problem remains fully live — self-determination requires a bounded demos. Migration scholars, UNHCR reporting, and comparative political theorists outside the beneficiary set argue the founding problem (coordinating self-governance) has been conflated with a separate, later-grafted function (labor market protection and demographic control) that does the actual work of the exclusion regime today, and that the foundational-authority framing forecloses proportionality review that even sovereignty-respecting frameworks elsewhere apply.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a genuine coordination function (bounded self-governance) coupled with a substantial, asymmetrically borne cost on non-members who have no standing to contest the boundary's legitimacy under this reading's own terms — that asymmetry is what makes it tangled rather than a clean rope. Suppression (0.72) is high because sovereignty-primary framing forecloses proportionality review as a matter of legal architecture, not merely as a matter of degree — the border's legitimacy does not depend on how the excluded are treated. Theater ratio is comparatively low (0.28) because enforcement substantially performs its stated function (actual exclusion occurs, actual self-governance is actually exercised) rather than being predominantly performative. Accessibility collapse (0.66) is high because, once a state's exclusion discretion is accepted as foundational rather than conditional, alternative claims (a right to entry, a proportionality test) are doctrinally foreclosed rather than merely disfavored. Resistance (0.61) is substantial and rising, reflecting growing international human-rights jurisprudence and migrant-rights advocacy contesting the sovereignty-primary framing itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizen polity and national labor incumbents sit near the beneficiary end: they retain full territorial access and, in the labor incumbents' case, plausible (if contested) wage protection, while bearing none of the direct costs of exclusion. Border enforcement apparatus sits as agenda_setter with institutional interests (budget, mandate growth) partially decoupled from the underlying self-determination claim — an override toward higher effective extraction on this seat would be warranted if evidence showed enforcement-budget growth outpacing actual cross-border pressure, but is not asserted here as the base case. Excluded migrants, asylum seekers, and stateless persons sit at the full-target end: trapped exit options, no legitimacy-contesting channel, direct and often severe cost-bearing. Displaced border communities are formally citizens (arguably beneficiaries) but experience the constraint as payers at the local level — their situation captures a within-polity asymmetry the sovereignty-primary framing does not itself address.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a bounded demos needs a way to constitute membership for self-governance) remains live in the sense that democratic self-rule below the global scale genuinely requires *some* membership-determination mechanism. But the sovereignty-primary reading's foundational (rather than conditional) framing of the exclusion power has drifted from solving that coordination problem into vindicating an enforcement apparatus whose scope (labor-market protection, demographic control, deterrent detention regimes) exceeds what self-governance coordination alone would require — this is exactly the divergence the tangled_rope classification is built to register: real coordination function, real asymmetric extraction, both present and load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_vs_conditional_authority,
    'Is the state''s authority to exclude non-members genuinely foundational (prior to and independent of any proportionality or rights balancing), or is it better characterized as a strong-but-conditional authority that this reading over-claims as foundational?',
    'Comparative doctrinal analysis across jurisdictions and international human rights bodies: track whether courts and tribunals that nominally endorse strong sovereignty claims nonetheless import proportionality review in practice (as the qualified_sovereignty sibling would predict) or genuinely treat the exclusion power as unreviewable (as this reading claims).',
    'If most legal systems that invoke sovereignty-primary rhetoric actually apply de facto proportionality tests, this reading''s foundational claim is largely rhetorical cover for a qualified_sovereignty practice — significantly narrowing this constraint''s real-world instantiation and shifting weight toward the qualified_sovereignty sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_vs_conditional_authority, conceptual, 'Whether sovereignty-primary is a genuine doctrinal position or a rhetorical gloss on qualified sovereignty in practice.').

omega_variable(
    labor_incumbent_benefit_empirics,
    'Does border exclusion actually produce net wage/employment benefits for national labor incumbents, or is this a widely believed but empirically thin justification?',
    'Labor economics literature on immigration''s wage effects, disaggregated by sector and skill level, compared against the political salience of the labor-protection justification for border enforcement.',
    'If the empirical benefit is thin or sector-specific, the beneficiary classification of national_labor_incumbents should be narrowed or treated as contested, weakening the coordination-function case for this reading''s tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_incumbent_benefit_empirics, empirical, 'Whether the claimed labor-market benefit to citizen workers is empirically robust.').

omega_variable(
    enforcement_apparatus_interest_capture,
    'Has the border enforcement apparatus''s institutional self-interest (budget growth, mandate expansion, detention-industry contracting) become a significant independent driver of exclusion policy, separate from the citizen polity''s actual self-determination interest?',
    'Track enforcement budget and detention capacity growth against measured cross-border flow volume and citizen-polity policy preferences over the same period; a persistent decoupling would indicate agenda-setter capture.',
    'If decoupling is substantial, a directionality_override raising the enforcement apparatus''s effective extraction profile (beyond simple agenda-setter neutrality) would be warranted in a future revision, and the tangled_rope classification''s asymmetric-extraction case strengthens further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_apparatus_interest_capture, empirical, 'Whether enforcement institutions have developed extraction interests independent of the underlying sovereignty claim.').

omega_variable(
    kernel_framing_selection,
    'Given that a border-normative-status kernel could be read as sovereignty_primary, qualified_sovereignty, or freedom_primary, what determines which reading a given state''s actual legal-political practice instantiates at a point in time?',
    'This is the committer-frame question itself: comparative constitutional and international law analysis of which reading''s axioms actually operate in a given jurisdiction''s border jurisprudence at a given historical moment, since jurisdictions can migrate between readings over time (e.g., a shift from sovereignty_primary toward qualified_sovereignty following human-rights treaty ratification).',
    'The classification of any real-world border regime as an instance of THIS reading versus a sibling reading depends on this resolution; misidentifying which reading a jurisdiction actually instantiates would misapply this constraint''s victim/beneficiary structure to a regime that is actually qualified_sovereignty or freedom_primary in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'How to determine which kernel reading a given real-world jurisdiction''s practice actually instantiates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__sovereignty_primary, theater_ratio, 0, 0.14).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__sovereignty_primary, theater_ratio, 8, 0.17).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__sovereignty_primary, theater_ratio, 16, 0.2).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__sovereignty_primary, theater_ratio, 24, 0.23).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__sovereignty_primary, theater_ratio, 32, 0.26).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__sovereignty_primary, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bord_be_t8, border_normative_status__sovereignty_primary, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(bord_be_t16, border_normative_status__sovereignty_primary, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(bord_be_t24, border_normative_status__sovereignty_primary, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(bord_be_t32, border_normative_status__sovereignty_primary, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(bord_be_t40, border_normative_status__sovereignty_primary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__sovereignty_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bord_su_t8, border_normative_status__sovereignty_primary, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(bord_su_t16, border_normative_status__sovereignty_primary, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(bord_su_t24, border_normative_status__sovereignty_primary, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(bord_su_t32, border_normative_status__sovereignty_primary, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(bord_su_t40, border_normative_status__sovereignty_primary, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the border_normative_status kernel, decomposed per the epsilon-invariance principle: sovereignty_primary (this file, tangled_rope — genuine coordination function plus asymmetric, doctrinally foreclosed extraction from excluded non-members), qualified_sovereignty (proportionality-bounded reading, expected lower extraction and lower suppression due to built-in rights balancing), and freedom_primary (movement-as-right reading, expected to classify border exclusion itself as substantially extractive/snare-like with citizens' polity interest as the qualified counterweight). Each reading has a distinct epsilon and distinct victim/beneficiary structure and must not be averaged together; they are linked here for contamination-propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
