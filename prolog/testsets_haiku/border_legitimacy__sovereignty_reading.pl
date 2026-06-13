% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Border Authority Derived from Territorial Sovereignty
 *   domain: political_philosophy/international_law
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested
 *   border-legitimacy kernel: the sovereignty reading. It models border
 *   authority as deriving legitimately from territorial sovereignty—the
 *   state's inherent right to control its territory and define membership.
 *   This reading grounds border exclusion in state self-determination and the
 *   coordination function of bounded political communities. The sibling
 *   readings (freedom-of-movement and humanitarian-obligation) contest the
 *   scope and legitimacy of this authority; those readings constitute
 *   separate constraint stories with different ε values, beneficiary/victim
 *   structures, and extracted populations. This story does not adjudicate the
 *   contest—it models what the sovereignty reading itself structurally
 *   entails: excluded migrants as victims, state authority as legitimate
 *   enforcement, and extraction concentrated on the powerless. The
 *   claim/metric gap is intentional: the reading claims tangled rope (genuine
 *   coordination function + asymmetric extraction through enforcement), while
 *   the authored metrics describe a substantially extractive constraint that
 *   persists primarily through suppression of alternatives and mobility.
 *
 * KEY AGENTS:
 *   - State apparatus: Sets and enforces border rules; collects sovereignty premium; has institutional power and analytical exit
 *   - Citizen body: Benefits from controlled membership and resource allocation; organized power but constrained exit
 *   - Excluded migrants: Powerless, trapped, denied entry; bear the constraint's extraction; have no voice in rules that exclude them
 *   - Asylum seekers: Powerless, identity-locked (fleeing forces them to seek asylum but seeking anchors their identity to states), bearing both coordination cost and extraction
 *   - Humanitarian NGOs: Excluded from sovereignty-framing; would contest the extraction but lack authority to set policy
 *   - International human-rights bodies: Analytical observers; witness enforcement but have no power to override state sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.72).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.79).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Border Authority Derived from Territorial Sovereignty").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '8e9a8395-08bf-45fd-a4cc-8b193b521b44').
narrative_ontology:cs_kernel_codification('8e9a8395-08bf-45fd-a4cc-8b193b521b44', formalized).
narrative_ontology:cs_authority_grounding('8e9a8395-08bf-45fd-a4cc-8b193b521b44', lineage).
narrative_ontology:cs_interpretation_layer_present('8e9a8395-08bf-45fd-a4cc-8b193b521b44').
narrative_ontology:cs_reading_relation('8e9a8395-08bf-45fd-a4cc-8b193b521b44', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('8e9a8395-08bf-45fd-a4cc-8b193b521b44', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('8e9a8395-08bf-45fd-a4cc-8b193b521b44', foundational, territorial_sovereignty_is_foundational).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('8e9a8395-08bf-45fd-a4cc-8b193b521b44', territorial_sovereignty_is_foundational, conventional).
narrative_ontology:cs_axiom('8e9a8395-08bf-45fd-a4cc-8b193b521b44', foundational, state_right_to_exclude_on_borders).
narrative_ontology:cs_axiom_status(state_right_to_exclude_on_borders, holdable).
narrative_ontology:cs_axiom_grounding('8e9a8395-08bf-45fd-a4cc-8b193b521b44', state_right_to_exclude_on_borders, deontological).
narrative_ontology:cs_reference_frame('8e9a8395-08bf-45fd-a4cc-8b193b521b44', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('8e9a8395-08bf-45fd-a4cc-8b193b521b44', contemporary_migration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8e9a8395-08bf-45fd-a4cc-8b193b521b44', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizen_body).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, asylum_seekers).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, receiving_state_bureaucracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state sets immigration policy, enforces border controls, determines who is admitted and under what conditions. Justifies exclusion as an expression of territorial sovereignty and the legitimate right to protect national institutions, labor markets, and welfare systems from unbounded access. Holds the authority to define citizenship and membership.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from the state's control over admission through preserved access to public goods (education, healthcare, welfare), labor-market protections, and the ability to define who belongs to the political community. Citizens exercise voice in border policy through democratic institutions (when these are functional). They also bear diffuse costs if exclusion generates humanitarian crises or if restrictive borders impede economic dynamism.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizen_body, beneficiary,
    organized, generational, constrained, national).

% Denied admission to territory and its institutions. May face persecution, economic destitution, or family separation as a result. Have no voice in the rules that exclude them and cannot exit the global political structure (all states claim sovereignty). Their only options are to comply with exclusion, attempt irregular entry at high risk, or be deported. Structurally unable to contest the legitimacy framework in which the border is embedded.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Flee persecution or disaster and seek refuge. International treaties nominally protect their right to apply for asylum, but states retain discretion over acceptance. They are trapped between the conditions that forced flight and the state's sovereign right to exclude. Identity-locked: their status as asylum-seeker is inseparable from their claim to entry; to abandon the identity is to abandon the possibility of legal protection.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, asylum_seekers, beneficiary).

% Border agents, immigration officers, and administrative bodies tasked with enforcing the exclusion. They carry out screening, detention, and deportation. May internalize the sovereignty frame or may experience moral conflict between enforcement duties and humanitarian obligations. Their exit is constrained by employment dependence and institutional hierarchy.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, receiving_state_bureaucracy, payer,
    moderate, biographical, constrained, national).

% Would argue for asylum acceptance and border permeability on grounds of human rights and humanitarian obligation. Advocate for alternatives to exclusion (intake capacity, integration support). Operate within the cracks of sovereignty but lack authority to set border policy. Marginalized from the sovereignty-based framing that dominates state decision-making.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, humanitarian_ngos, excluded,
    moderate, biographical, mobile, global).

% Monitor state compliance with human rights treaties (right to life, freedom from torture, right to asylum). Observe and report on border enforcement practices. Have no direct enforcement power over states and operate in a framework where states retain final sovereignty. Their role is witness and advocacy, not authority.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__sovereignty_reading, state_apparatus).
narrative_ontology:fixing_cost_class(border_legitimacy__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable rule space: states can plan institutions and public goods allocation on the assumption of controlled membership. Solves the coordination problem of defining who is included in a bounded political community—citizens can reasonably expect welfare systems and labor-market rules to apply to a knowable set of people. Without border authority, no state could guarantee conditions of membership.
% TRANSFER_FUNCTION: Transfers exclusion from territory, public goods access, and formal economic participation FROM excluded migrants and asylum seekers TO the state apparatus and citizen body. The state collects the authority premium (the right to exclude); citizens collect the benefit of limited-set membership. The cost is borne by those denied admission.
% ABSENT_VOICES: Excluded migrants and asylum seekers are structurally absent from the decision-making frame. They cannot vote on immigration policy, cannot sit in legislatures, and cannot contest the sovereignty axiom itself—they are the subject of rules they cannot influence. Humanitarian organizations and human-rights bodies would argue for admission standards and capacity-building rather than categorical exclusion, but are marginalized from the sovereignty-based framing. Future generations who would inherit climate-displaced populations are also absent.
% DISAPPEARANCE_RATIONALE: If border authority and territorial sovereignty collapsed overnight, states would lose their primary mechanism for controlling membership and resource allocation. Welfare systems would face immediate pressure from unrestricted entry. Labor markets would reorganize. The political structure of nation-states themselves—institutions built on the assumption of bounded membership—would face fundamental reconfiguration. Successor institutions (whether regional federations, city-states, or open-borders frameworks) would need to solve the coordination problem of membership differently. The world would rearrange because state capacity depends on this constraint.
% FOUNDING_PROBLEM: In the post-Westphalian era, states needed a doctrine that granted them exclusive authority over territory, population, and institutions within borders. Without it, competing powers would fight over the same populations and resources. Territorial sovereignty solved the anarchic competition problem by dividing the globe into exclusive zones of state authority. Each state claimed the right to determine who could enter its territory and reside there.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars attest that the Westphalian system did solve the territorial fragmentation problem historically. States themselves defend sovereignty as the foundation of international order. However, human-rights bodies, humanitarian organizations, and academic critics from outside the state apparatus argue that the founding problem (anarchic competition for territory) is decoupled from the current use of borders (which now primarily restrict poor and displaced populations from richer states). Climate scientists and development economists attest that the premise of stable, bounded populations on which Westphalian institutions were built no longer holds empirically. The founding problem's persistence is contested: its original form (great-power territorial competition) has partially shifted to other domains, while borders persist primarily as gatekeeping mechanisms for resource access.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 endpoint) because the constraint's operation systematically denies the powerless (excluded migrants) access to resources and territory while the state collects unilateral authority over admission. The constraint does coordinate a real function—defining stable membership for welfare and institutions—but does so by extracting from those excluded. Suppression is higher still (0.79) because the constraint's persistence depends on active enforcement: detention, deportation, border fortification, exclusion of asylum claims. Theater is moderate (0.28): some enforcement activity is genuinely protective (screening), but an increasing share defends the categorical exclusion principle rather than addressing genuine security or integration challenges. The measurement series shows extractiveness accumulating as migration pressure increases (t0=0.58 to t25=0.72), consistent with states hardening exclusion in response to demand for entry. Suppression requirement also rises, indicating enforcement machinery intensifies as resistance grows. Theater ratio drifts upward slightly, consistent with performative border infrastructure (walls, ceremonial crackdowns) accompanying the extraction accumulation.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and citizen seats should compute as experiencing this constraint very differently than the excluded-migrant seats. From the institutional seat, the constraint is genuine coordination (stable membership enables public goods). From the powerless, excluded seat, it is pure extraction: denied access to territory, livelihood, and institutional protection. The engine computes this divergence from the structural data—the beneficiary/victim split, the power asymmetry, the identity-locked exit. The sovereignty reading CLAIMS this is legitimate authority; the computation will show whether the structural data align with that claim or whether the metrics describe extraction masquerading as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: d near beneficiary end (~0.1–0.2). It sets the rules, collects the premium, has institutional power and analytical exit (can reframe or redeploy authority). Citizen body: d slightly toward target end (~0.4–0.5). Benefits from membership control but bears diffuse costs if exclusion generates humanitarian crises or if restrictive borders trap skilled workers abroad. Excluded migrants and asylum seekers: d near full target end (~0.85–0.95). Trapped, denied access, no voice, bearing the extraction directly. Identity-locked asylum seekers sit slightly lower on the target end than purely economic migrants because international treaties nominally protect their admission—but the sovereignty reading asserts states retain discretion, pushing them back toward the target. Humanitarian NGOs: d analytical (they critique but don't participate in the extraction). The override at institutional seats is minimal here; the structural derivation from power + exit + beneficiary/victim captures the directionality well.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was anarchic territorial competition between great powers in the 17th–20th centuries. The founding_problem_status is 'contested' because the original problem (great-power competition for territory) has partially shifted—security now depends more on economic and ideological competition—while borders persist primarily as mechanisms for controlling labor-market access and resource distribution. This is mandatrophy at work: the constraint's justification (security through territorial control) has decoupled from its operation (exclusion of poor and displaced populations). The sovereignty reading must acknowledge that the founding problem has changed. A constraint that persists despite its founding problem decoupling is a candidate for reclassification or sunset. However, the sovereignty reading does not itself resolve the mandatrophy; that resolution belongs to the full kernel analysis across all three readings. This story documents what the sovereignty reading structurally entails; it does not claim the reading is the only coherent one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_decoupling,
    'Has the founding problem (anarchic territorial competition between states) that justified border sovereignty changed enough that the sovereignty reading no longer rests on its original justification?',
    'Comparative analysis of contemporary state security threats (great-power war, regional conflict, economic competition, migration, climate) against the 17th-century Westphalian problem; assessment of whether current border enforcement addresses the threats that originally justified territorial control.',
    'If the founding problem has substantially shifted or been solved in other domains, the sovereignty reading faces mandatrophy: the constraint persists but its justification is obsolete. This would support reclassification toward piton (inertial persistence of an atrophied function) or creation of a sunset clause. If the founding problem remains live (great-power competition still driven by territorial control), the sovereignty reading maintains its justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_decoupling, empirical, 'Whether territorial sovereignty doctrine still addresses the problem it was created to solve.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination function (stable membership enabling institutions) structurally inseparable from the exclusionary extraction, or could membership be bounded in other ways (by law, cultural affiliation, contribution) without categorical territorial denial?',
    'Counterfactual: jurisdictions that bound membership by criteria other than birthright (e.g., long-term residency, civic participation, economic contribution) and observe whether institutional stability persists. Natural experiments from federal systems that admit subnational mobility.',
    'If separable, the sovereignty reading''s claim that border exclusion is necessary for coordination is false; the extraction is contingent rather than essential. If inseparable, the sovereignty reading is stronger—the extraction is the price of the coordination. This omega maps directly onto the sibling readings: humanitarian and freedom-of-movement readings would assert separability; sovereignty reading asserts inseparability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether stable membership can be achieved without categorical territorial exclusion.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.79) primarily structural (borders, deportation, legal barriers) or internalized (migrants internalize exclusion as legitimate, accept borders as natural, reduce migration attempts in response to normalized rejection)?',
    'Post-border-policy-change analysis: if suppression requirement drops substantially when enforcement is loosened (or rises if it tightens), suppression is primarily structural. If suppression persists even after barriers are lowered, it is partially internalized—migrants have incorporated the exclusion frame.',
    'If internalized, the constraint''s effective suppression is higher than structural measures alone suggest—victims carry the suppression with them across borders and generations. If structural, the suppression can be reduced by changing enforcement policy. This informs whether the constraint is primarily mechanical or ideological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether border suppression is maintained by external enforcement or internalized as legitimate.').

omega_variable(
    sovereignty_reading_vs_sibling_foreclosure,
    'Does the sovereignty reading''s core axiom (states have legitimate right to exclude on grounds of territorial sovereignty) logically foreclose the freedom-of-movement reading''s core axiom (freedom of movement is a human right), or do the readings coexist as different parties'' commitments?',
    'Analytical: does any single party (state, individual, legal framework) hold both axioms simultaneously without contradiction? Or do they map to incompatible frameworks?',
    'If foreclosing: the sovereignty and freedom-of-movement readings cannot both be true in any coherent framework—one must be abandoned. The engine computes this as foreclosure/3 relationship. If coexisting: the readings map to incompatible frameworks (state sovereignty vs. individual rights) held by different parties; both remain live. This omega documents the kernel-reading relationship itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_reading_vs_sibling_foreclosure, conceptual, 'Whether sovereignty and freedom-of-movement readings are logically incompatible or merely contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_legitimacy__sovereignty_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_legitimacy__sovereignty_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_legitimacy__sovereignty_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_legitimacy__sovereignty_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_legitimacy__sovereignty_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(bord_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__sovereignty_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_legitimacy__sovereignty_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_legitimacy__sovereignty_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_legitimacy__sovereignty_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_legitimacy__sovereignty_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_legitimacy__sovereignty_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement_basis(bord_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__sovereignty_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_legitimacy__sovereignty_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_legitimacy__sovereignty_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_legitimacy__sovereignty_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_legitimacy__sovereignty_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_legitimacy__sovereignty_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(bord_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the border-legitimacy kernel. The sovereignty_reading asserts territorial authority as legitimate; the freedom_of_movement_reading asserts borders as presumptively illegitimate restrictions on human rights; the humanitarian_obligation_reading asserts a middle path (admission obligation for refugees, but not general migrants). These three constraints share a kernel (border authority) but instantiate different ε values, beneficiary/victim structures, and victim populations. Each story must be read independently; the kernel contest is the relationship between stories, not within any single story. The network links capture the structural influence: sovereignty reading influences both siblings by establishing the authority framework they contest; humanitarian reading influences both by proposing an intermediate standard; freedom-of-movement reading forecloses sovereignty reading if individual human rights are treated as foundational.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
