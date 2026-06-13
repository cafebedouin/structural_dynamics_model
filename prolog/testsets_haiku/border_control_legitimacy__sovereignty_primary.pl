% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Border Control as Sovereignty Enforcement (Sovereignty-Primary Reading)
 *   domain: political/legal
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'border_control_legitimacy.' The sovereignty-primary reading asserts that
 *   state territorial sovereignty entails absolute discretion to exclude
 *   non-citizens, and that border control is constitutive of statehood
 *   itself. This is not the only reading — a freedom-of-movement-primary
 *   reading treats exclusion authority as incompatible with human rights; a
 *   jurisdictional-sovereignty reading separates the state's authority to
 *   regulate internal affairs from its authority to exclude borders entirely.
 *   Each reading is a structurally distinct constraint with its own ε,
 *   beneficiary/victim structure, and classification. The authoring task is
 *   to instantiate THIS reading cleanly, without bleeding into the others.
 *   The claim/metric independence rule applies: this story is CLAIMED as
 *   tangled_rope (genuine coordination function in establishing political
 *   community, plus asymmetric extraction via excluded migrants) while the
 *   metrics describe substantially extractive operation (extractiveness 0.68,
 *   suppression 0.76) with rising theater over the post-WWII interval (0.22 →
 *   0.42). The engine will compute each seat's type from the structural data
 *   — agenda-setter institutional, citizen beneficiary/payer, excluded
 *   migrant payer/victim — independently of the claim.
 *
 * KEY AGENTS:
 *   - State administrative apparatus (institutional, agenda-setter): controls border machinery, citizenship determination, enforcement; claims sovereignty requires absolute discretion over membership
 *   - Citizen body (organized, beneficiary/payer): benefits from privileged access and membership stability; bears diffuse enforcement costs
 *   - Excluded migrants (powerless, payer/victim): denied entry; no recourse; trapped outside jurisdiction that shapes their fate
 *   - Internal migrant populations (moderate, payer): subject to deportation, subordinate labor status, permit bureaucracies justified by sovereignty principle
 *   - Rival sovereignty claimants (institutional, excluded): human rights bodies, supra-national authorities, labor-dependent states whose claims are structurally excluded
 *   - Academic observers (analytical, observer): examine whether the reading's foundational axiom survives empirical and normative scrutiny
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.68).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.76).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Border Control as Sovereignty Enforcement (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political/legal").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '1f57ce23-f99b-4896-8d38-95ebecc300e9').
narrative_ontology:cs_kernel_codification('1f57ce23-f99b-4896-8d38-95ebecc300e9', fixed_text).
narrative_ontology:cs_authority_grounding('1f57ce23-f99b-4896-8d38-95ebecc300e9', extraction).
narrative_ontology:cs_interpretation_layer_present('1f57ce23-f99b-4896-8d38-95ebecc300e9').
narrative_ontology:cs_reading_relation('1f57ce23-f99b-4896-8d38-95ebecc300e9', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('1f57ce23-f99b-4896-8d38-95ebecc300e9', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('1f57ce23-f99b-4896-8d38-95ebecc300e9', foundational, border_closure_constitutive_statehood).
narrative_ontology:cs_axiom_status(border_closure_constitutive_statehood, holdable).
narrative_ontology:cs_axiom_grounding('1f57ce23-f99b-4896-8d38-95ebecc300e9', border_closure_constitutive_statehood, deontological).
narrative_ontology:cs_axiom('1f57ce23-f99b-4896-8d38-95ebecc300e9', foundational, absolute_exclusion_discretion).
narrative_ontology:cs_axiom_status(absolute_exclusion_discretion, holdable).
narrative_ontology:cs_axiom_grounding('1f57ce23-f99b-4896-8d38-95ebecc300e9', absolute_exclusion_discretion, conventional).
narrative_ontology:cs_reference_frame('1f57ce23-f99b-4896-8d38-95ebecc300e9', westphalian_sovereign_statehood).
narrative_ontology:cs_drift_state('1f57ce23-f99b-4896-8d38-95ebecc300e9', contemporary_supranational_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1f57ce23-f99b-4896-8d38-95ebecc300e9', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_body).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, state_administrative_apparatus).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, internal_migrant_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, citizen_body).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_sovereign_statehood).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, territorial_exclusivity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers border control, citizenship determination, and exclusion enforcement. Justifies these powers as constitutive of statehood itself — the capacity to determine who may enter and reside is presented as inseparable from the right to self-determination. Controls the legal machinery, border infrastructure, and deportation apparatus. Claims that relinquishing absolute border discretion would cede sovereign capacity to external actors and international bodies.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from the state's claimed monopoly on defining membership and setting admission rules — citizens retain privileged access to labor markets, social benefits, and political voice. They also bear diffuse costs: the enforcement apparatus consumes resources (border infrastructure, detention, monitoring), and internal policing of migration status creates ambient surveillance friction. Citizens are presented as the constituency whose collective will the border apparatus represents.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_body, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, citizen_body, payer).

% Are denied entry based on origin, skill, or wealth. They face the apparatus's force directly — detention, deportation, criminalization, family separation. Their exclusion is the central enforcement object; the border regime exists to prevent their entry and passage. They have no recourse within the state's legal system (their standing is denied by the same sovereignty premise that excludes them) and are trapped by geography and circumstance outside the jurisdiction that controls their fate.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Are persons inside the territorial state — often documented migrants, refugees admitted under humanitarian frames, or citizens of irregular descent — whose status remains subordinate to the sovereignty framework. They are subject to deportation, permit revocation, and subordinate labor protections justified by the principle that the state has absolute discretion over their presence. They bear enforcement costs (status verification, permit bureaucracies, workplace raids) and labor market subordination (used as wage-pressure mechanisms, denied full rights).
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, internal_migrant_populations, payer,
    moderate, biographical, constrained, national).

% Include international human rights bodies, regional supra-national authorities, and other states whose claims to regulate migration intersect with this state's borders (diaspora states, labor-dependent neighboring states, refugee-burden states). They are structurally excluded from the agenda — the sovereignty-primary reading treats their claims as external, not constitutive of legitimate authority. They would argue for rights-based limits on exclusion or shared migration governance; the enforcement apparatus exists partly to ward off their influence.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, rival_sovereignty_claimants, excluded,
    institutional, generational, trapped, global).

% Study the constraint from outside — philosophers of sovereignty, migration scholars, international law experts. They examine whether the reading's foundational claim (border closure is constitutive of statehood) survives empirical and normative scrutiny, whether it forecloses rival readings, and whether its enforcement costs exceed its coordination benefits.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, academic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bounded political community with defined membership: citizens collectively determine admission rules through democratic processes (in theory); the state coordinates citizenship, labor market access, and welfare eligibility around a stable in-group. This solves the problem of coordinating collective goods and rights allocation — someone must decide who is in the polity and who participates in wealth distribution.
% TRANSFER_FUNCTION: Moves enforcement costs (surveillance, detention, deportation, border infrastructure) from the state apparatus to excluded migrants and internal migrant populations, who bear the weight of exclusion and subordination. It also transfers labor-market benefits from excluded migrants (who cannot compete openly) to citizens, who retain privileged access. A secondary transfer: political power and voice remain monopolized by the citizen body — the decision-making apparatus that controls border rules is unavailable to those it affects most directly.
% ABSENT_VOICES: Excluded migrants have no seat at the table — they are the constraint's primary targets but cannot participate in the sovereignty-based reasoning that justifies their exclusion (a structural circularity: the sovereignty framework denies their standing to challenge it). International human rights bodies, rival states, and transnational migrant organizations are similarly excluded from the agenda, treating their claims as external pressures rather than constitutive of legitimate authority.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primary reading's enforcement vanished overnight — if states lost the capacity and will to exclude — migration patterns would shift rapidly, labor markets would equilibrate at different wage levels, citizenship would lose its exclusive economic rent, and political communities would reorganize around different membership criteria (possibly place-based, occupation-based, or rights-based rather than citizenship-based). The nation-state as currently constituted depends on border control's continuous operation.
% FOUNDING_PROBLEM: The problem is the coordination and stability of political community: how to define a bounded collective that makes decisions together, distributes collective goods, and maintains territorial control without continuous internal coercion. The sovereignty-primary reading asserts that border control — the capacity to determine membership from the outside — is the foundational answer.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and nationalist political parties attest the founding problem is live — borders are necessary to maintain political coherence and protect citizens. Migration scholars and human rights advocates attest the founding problem has been reframed: the real problem is not membership stability but rather the cost-shifting to excluded populations and the loss of exit options for the powerless. Empirical evidence from open-border regions (Schengen, freedom-of-movement zones) offers mixed corroboration: some research shows political community persists without hard borders; other work shows new forms of inequality and governance emerge. No consensus from outside the benefiting parties.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.68) reflects a constraint that moves substantial rents from excluded/subordinate migrants to citizens and the state apparatus, justified by a sovereignty claim. The suppression score (0.76) is high because the constraint's persistence depends on actively excluding rival sovereignty claimants (international human rights bodies, transnational movement advocates) and maintaining the legal fiction that excluded migrants lack standing to challenge the framework. The theater_ratio's rise (1945: 0.22 → 2025: 0.42) indicates that enforcement activity increasingly defends border exclusivity itself rather than genuine coordination — early in the post-WWII period, border control did provide meaningful security and order functions (lower theater); by 2025, amid open-border zones (EU Schengen), digital migration, and asylum crises, the performance-to-function ratio has risen as the coordination justification weakens and pure exclusion enforcement hardens. The accessibility_collapse score (0.79) reflects that once the sovereignty framework is internalized, alternatives to border control collapse: citizens view open borders as threatening (state annihilation), migrants view exclusion as natural law (not contested). The resistance score (0.71) captures growing activist opposition to border enforcement, human rights challenges, and scholarly critique, which the apparatus must actively suppress. One time grid across all metrics: every metric is authored at every examined point (1945, 1965, 1985, 2005, 2015, 2025) so temporal analysis has a complete alignment.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and citizen body should compute differently than excluded migrants. The apparatus sees the constraint as coordinate-and-cooperate (we establish political community through membership rules), while excluded migrants see it as pure extraction (we are trapped outside a jurisdiction that controls our fate without our voice). Citizens occupy a hybrid seat: they are beneficiaries of privileged access and coordinated public goods, but also payers of enforcement costs and ambient surveillance. The engine computes per-seat directionality from the beneficiary/victim declarations and exit options: apparatus = beneficiary, no exit (d toward 0.0, subsidy); citizens = mixed (beneficiary for goods, payer for enforcement costs — the secondary_role captures this duality); excluded migrants = victim, trapped exit (d toward 1.0, maximum target). The divergence in computed types (apparatus as rope-beneficiary, migrants as snare-victim) is the signal the corpus measures — the divergence is not an error, it is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus benefits from the constraint and controls the rules that sustain it — declaratively a beneficiary, structurally d near 0.0 (subsidy framing). Citizens benefit from privileged access (low d) but bear enforcement costs (higher d) — declared dual-role (beneficiary/payer) captures the asymmetry. Excluded migrants are the primary victims (high d toward 1.0, pure target) — they are denied entry, have no exit (trapped), and have no standing in the system that governs them. Internal migrant populations occupy a similar position but with slightly more structural options (they are at least inside the jurisdiction, can potentially regularize) — still high d but not quite the full target of excluded migrants. The directionality overrides are not needed here; the structural derivation from beneficiary/victim + exit options produces the correct d values. The sovereignty-primary reading is not authored with an override; if the empirical challenge omega resolves (open borders persist and states survive), a future editor might add an override for the apparatus to reflect regulatory capture by supranational bodies, but the current structural data supports the derived d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (membership stability, political community coordination) is disputed (founding_problem_status = contested). The disappearance verdict is world_rearranges — if border control enforcement ceased, the nation-state system as currently constituted would reorganize. However, the theater_ratio's rise (0.22 → 0.42) suggests that the coordination function is attenuating while the extraction defense is hardening. The interpretation-layer present (interpretation_layer_present = true under authority_grounding = extraction) indicates that the state apparatus maintains the sovereignty principle through legal doctrine, constitutional interpretation, and nationalist narrative even as empirical challenges accumulate (Schengen, freedom-of-movement zones, asylum supranationalism). The theater rise maps to Goodhart drift: as the founding coordination problem weakens (political communities persist without hard borders; rival mechanisms emerge), enforcement becomes increasingly about defending extraction (labor-market rents, political power monopoly) rather than about the original coordination goal. The mandatrophy is NOT yet resolved — the constraint still claims coordination function and still enforces it, even as the coordination necessity is contested. A future evolution: if founding_problem_status drifts to dead while disappearance_verdict stays world_rearranges, the constraint crosses into zombie territory and mandatrophy becomes active (the founding problem is gone but the apparatus persists).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_foreclosure,
    'Does the sovereignty-primary reading''s core axiom (border closure is constitutive of statehood) logically foreclose the freedom-of-movement-primary reading''s core axiom (freedom of movement is a fundamental human right), or do both remain holdable within different frameworks?',
    'Philosophical analysis and jurisprudential comparison: do the two readings occupy incommensurable conceptual spaces (foreclosure) or different seats at the same decision table (coexistence)? Do any major courts, legal traditions, or political systems maintain BOTH axioms simultaneously without contradiction?',
    'If foreclosure: the readings are fundamentally incompatible; one must win and one must lose at the kernel level. If coexistence: both readings remain live political positions, and the constraint is one seat in an ongoing dispute, not the settled answer. The classification of both sibling constraints depends on this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Whether this reading''s core claim forecloses or coexists with the freedom-of-movement reading').

omega_variable(
    constitutive_vs_instrumental_statehood,
    'Is border-control authority constitutive of statehood (inseparable from what it means to be a state) or instrumental to statehood (a useful tool states have typically employed, but not essential to state existence)?',
    'Historical and comparative analysis: examine states that have voluntarily relinquished absolute border control (EU member states in the Schengen zone, Nordic passport unions) — do they remain states? Do they lose sovereignty? What alternative mechanisms coordinate citizenship and belonging in these cases?',
    'If constitutive: the sovereignty-primary reading''s foundational claim stands; border closure is inseparable from legitimate state authority. If instrumental: the reading conflates a historical habit with a logical necessity; other mechanisms (place-based rights, residency-based suffrage, voluntary association) can coordinate belonging, which would support the jurisdictional-sovereignty reading instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_instrumental_statehood, empirical, 'Whether border control is essential to statehood or contingent').

omega_variable(
    internal_extraction_mechanism,
    'How much of the measured extraction (0.68) comes from the coordination benefit (membership stability, collective-goods provision to citizens) and how much comes from the pure rent-seeking (labor-market advantage for citizens via excluded migrants'' blocked supply, political power monopoly)?',
    'Decomposition analysis: measure the citizen body''s welfare gains attributable to access to coordinated public goods separately from their welfare gains attributable to labor-market scarcity rents and political power. Compare to counterfactual scenarios with open borders but maintained public-goods coordination.',
    'A high rent/coordination ratio (60%+ of extraction is rent) would support reclassification toward snare; a low ratio (20%- of extraction is rent) would support the rope/coordination reading. The theater_ratio''s rise (0.22 to 0.42 over the interval) suggests increasing performance relative to function, which implies rent is accumulating.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_extraction_mechanism, empirical, 'Proportion of extraction attributable to rent-seeking vs. coordination function').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.76) primarily structural (legal barriers, geographic isolation, economic exclusion of undocumented migrants) or internalized (migrants internalize unwantedness, adopt subordinate identities, self-select out of resistance)?',
    'Post-exit suppression trajectory: examine migrant populations who successfully exit (naturalization, legal status gain, relocation to open-border zones) — does suppression persist? If suppression remains after the structural barrier is removed, it is partially internalized; if it dissipates, it is primarily structural.',
    'If primarily structural: the constraint''s suppression is lower than it appears because it disappears with the exit. If partially internalized: the constraint carries hidden persistence costs — excluded migrants retain suppressed self-concepts even after structural exclusion ends, which suggests deeper identity fusion and more extractive operation than the scalar suppression measure captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural barriers or internalized identity subordination').

omega_variable(
    sibling_reading_empirical_challenge,
    'What empirical developments would constitute a challenge to this reading''s axiom that border closure is constitutive of statehood? Conversely, what would confirm it?',
    'Identify and monitor: successful long-term open-border unions that maintain internal political coherence and public-goods provision; states that lose statehood status or political stability after border liberalization; new forms of political community that coordinate belonging without territorial exclusivity.',
    'Empirical challenge could route through the T17 abductive trigger (mountain_extraction_accumulation for mountains; here it would be axiom_empirical_challenge for a commitment-system reading): if the founding assumption fails empirically, the reading drifts toward overridden status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_challenge, empirical, 'Empirical conditions that would refute or confirm the constitutive-statehood axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_control_legitimacy__sovereignty_primary, theater_ratio, 1945, 0.22).
narrative_ontology:measurement_basis(bord_tr_t1945, observed).
narrative_ontology:measurement(bord_tr_t1965, border_control_legitimacy__sovereignty_primary, theater_ratio, 1965, 0.26).
narrative_ontology:measurement_basis(bord_tr_t1965, observed).
narrative_ontology:measurement(bord_tr_t1985, border_control_legitimacy__sovereignty_primary, theater_ratio, 1985, 0.31).
narrative_ontology:measurement_basis(bord_tr_t1985, observed).
narrative_ontology:measurement(bord_tr_t2005, border_control_legitimacy__sovereignty_primary, theater_ratio, 2005, 0.37).
narrative_ontology:measurement_basis(bord_tr_t2005, observed).
narrative_ontology:measurement(bord_tr_t2015, border_control_legitimacy__sovereignty_primary, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(bord_tr_t2015, observed).
narrative_ontology:measurement(bord_tr_t2025, border_control_legitimacy__sovereignty_primary, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(bord_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement_basis(bord_be_t1945, observed).
narrative_ontology:measurement(bord_be_t1965, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement_basis(bord_be_t1965, observed).
narrative_ontology:measurement(bord_be_t1985, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1985, 0.51).
narrative_ontology:measurement_basis(bord_be_t1985, observed).
narrative_ontology:measurement(bord_be_t2005, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(bord_be_t2005, observed).
narrative_ontology:measurement(bord_be_t2015, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement_basis(bord_be_t2015, observed).
narrative_ontology:measurement(bord_be_t2025, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(bord_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1945, 0.38).
narrative_ontology:measurement_basis(bord_su_t1945, observed).
narrative_ontology:measurement(bord_su_t1965, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement_basis(bord_su_t1965, observed).
narrative_ontology:measurement(bord_su_t1985, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement_basis(bord_su_t1985, observed).
narrative_ontology:measurement(bord_su_t2005, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement_basis(bord_su_t2005, observed).
narrative_ontology:measurement(bord_su_t2015, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement_basis(bord_su_t2015, observed).
narrative_ontology:measurement(bord_su_t2025, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2025, 0.76).
narrative_ontology:measurement_basis(bord_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__sovereignty_primary, 0.12).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'border_control_legitimacy.' The freedom_of_movement_primary and jurisdictional_sovereignty constraints are sibling readings with different ε values, beneficiary/victim structures, and classifications. They share the same kernel (the contested claim about what legitimate statehood entails) but instantiate different structural relationships. All three stories must be linked via network.affects_constraints to show the family structure; the kernel context field in commentary explains the decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
