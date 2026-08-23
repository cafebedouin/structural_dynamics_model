% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Intermediate-Channels Reading of the Federal Commerce Power
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   A constitutional allocation fixes the reach of federal legislative power
 *   over the national economy along three paths — the routes of interstate
 *   trade, the vehicles and persons moving through them, and activities with
 *   substantial cumulative effects on that trade — while categorical limits
 *   reserve non-economic local conduct to the states unless a statute carries
 *   an explicit interstate nexus, confine aggregation arguments to economic
 *   activity, and forbid regulation through long causal chains. Enforcement
 *   runs through judicial review: statutes exceeding the lines are struck,
 *   and comprehensive schemes within them stand even where they reach wholly
 *   local instances. Federal institutions gain sweeping authority over the
 *   economic field; state governments keep family law, criminal justice, and
 *   education so long as conduct stays local and non-commercial, but cede the
 *   economic sphere and pay to defend the line; interstate firms gain uniform
 *   national rules; people harmed by conduct the lines exclude lose access to
 *   any federal forum. KEY AGENTS (by structural relationship):
 *   federal_government — agenda-setting beneficiary
 *   (institutional/constrained); supreme_court — co-administrator of the line
 *   (institutional/analytical); state_governments — dual-positioned holder of
 *   the reserved sphere and payer inside the ceded one (organized/trapped);
 *   interstate_businesses — uniformity beneficiary (powerful/arbitrage);
 *   local_harm_victims — forum-excluded payers (powerless/trapped);
 *   intrastate_medical_providers — swept-in payers (powerless/trapped);
 *   constitutional_legal_academy — analytical observer;
 *   ordinary_regulated_public — absent voice (powerless/trapped).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.46).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.58).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.46).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Intermediate-Channels Reading of the Federal Commerce Power").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "legal/constitutional").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, '3c84ec87-65f2-4462-bf76-55dbc5b68cde').
narrative_ontology:cs_kernel_codification('3c84ec87-65f2-4462-bf76-55dbc5b68cde', fixed_text).
narrative_ontology:cs_authority_grounding('3c84ec87-65f2-4462-bf76-55dbc5b68cde', lineage).
narrative_ontology:cs_interpretation_layer_present('3c84ec87-65f2-4462-bf76-55dbc5b68cde').
narrative_ontology:cs_reading_relation('3c84ec87-65f2-4462-bf76-55dbc5b68cde', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('3c84ec87-65f2-4462-bf76-55dbc5b68cde', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_axiom('3c84ec87-65f2-4462-bf76-55dbc5b68cde', foundational, categorical_limits_bind_commerce_power).
narrative_ontology:cs_axiom_status(categorical_limits_bind_commerce_power, holdable).
narrative_ontology:cs_axiom_grounding('3c84ec87-65f2-4462-bf76-55dbc5b68cde', categorical_limits_bind_commerce_power, conventional).
narrative_ontology:cs_axiom('3c84ec87-65f2-4462-bf76-55dbc5b68cde', foundational, state_police_powers_constitutionally_reserved).
narrative_ontology:cs_axiom_status(state_police_powers_constitutionally_reserved, holdable).
narrative_ontology:cs_axiom_grounding('3c84ec87-65f2-4462-bf76-55dbc5b68cde', state_police_powers_constitutionally_reserved, deontological).
narrative_ontology:cs_axiom('3c84ec87-65f2-4462-bf76-55dbc5b68cde', secondary, jurisdictional_elements_gate_noneconomic_regulation).
narrative_ontology:cs_axiom_status(jurisdictional_elements_gate_noneconomic_regulation, holdable).
narrative_ontology:cs_axiom_grounding('3c84ec87-65f2-4462-bf76-55dbc5b68cde', jurisdictional_elements_gate_noneconomic_regulation, instrumental).
narrative_ontology:cs_reference_frame('3c84ec87-65f2-4462-bf76-55dbc5b68cde', tripartite_paths_with_categorical_limits).
narrative_ontology:cs_drift_state('3c84ec87-65f2-4462-bf76-55dbc5b68cde', contemporary_post_raich_era, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('3c84ec87-65f2-4462-bf76-55dbc5b68cde', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, interstate_businesses).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, local_harm_victims).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, intrastate_medical_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and enforces legislation over the national economy through the three authorized paths; must attach explicit interstate-nexus provisions whenever a bill touches conduct that is not itself economic; operates comprehensive regulatory schemes covering wages, environmental floors, food and drug safety, and controlled substances that reach purely local instances through their place in the national picture; defends the reach of its statutes when they are challenged in court.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Decides which statutes stay within the three paths and whether the categorical limits were honored; has struck a school-zone gun penalty and a federal civil remedy for gender-motivated violence for exceeding the lines, and has sustained comprehensive national schemes that sweep in local instances; maintains the precedent stack that later Congresses and lower courts must navigate; bears the institutional cost of policing a boundary that organized interests continuously probe from both directions.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Retain primary authority over family law, criminal justice, and education so long as the conduct at issue stays local and non-commercial, and won judicial confirmation of that reservation in the decisions that first drew the modern lines. At the same time, every economic activity inside their borders is federally regulable in principle, so their economic-policy space shrinks to whatever Congress has not occupied; they litigate the boundary in multi-state coalitions, absorb federal standards without matching fiscal support, and cannot secede from or opt out of the arrangement.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_governments, beneficiary,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, state_governments, payer).

% Operate under one set of federal rules from coast to coast instead of fifty divergent regimes, lowering multi-state compliance costs and making nationwide product and workforce planning feasible. They carry federal compliance burdens and exposure to comprehensive schemes that reach individual facilities through aggregate-effects logic; they lobby at the margin for more uniformity in some markets and carve-outs in others, and can restructure or relocate when the cost calculus turns.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, interstate_businesses, beneficiary,
    powerful, biographical, arbitrage, national).

% People injured by conduct that is local and non-economic — gender-motivated violence being the case that defined the line — lost the federal civil remedy Congress had written for them when the courts held the statute exceeded the authorized paths. Their recourse reverted to state systems whose protections vary widely; from where they stand, the categorical reservation is experienced as a closed door to the stronger federal forum, and no relocation or restructuring restores it.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, local_harm_victims, payer,
    powerless, biographical, trapped, local).

% Grow and distribute medicine strictly within one state, without cash sale, for patient use; because a comprehensive federal scheme governs the same substance nationally, their wholly local, non-commercial activity is treated as part of that scheme's economic picture and is prosecuted and destroyed. Exit would mean abandoning the patients they serve; their supply chains and clientele are fixed in place.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, intrastate_medical_providers, payer,
    powerless, biographical, trapped, local).

% Studies, criticizes, and proposes reconstructions of the allocation; trains the judges, clerks, and staff who administer it; documents where the lines hold firm and where statutory drafting routes around them. Collects nothing from and bears nothing of the arrangement directly; its stake is interpretive authority over the tradition.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, constitutional_legal_academy, observer,
    analytical, generational, analytical, national).

% Live inside the resulting allocation — working, consuming, and seeking safety under rules they did not negotiate. Whether a harm counts as federable, or a workplace standard is set nationally or left local, is decided in courtrooms and conference rooms they do not occupy; their voice arrives only through elections and interest organizations, filtered through the very institutions the allocation empowers.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, ordinary_regulated_public, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:fixing_cost_class(commerce_clause_scope__intermediate_channels, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides uniform, enforceable rules for an integrated national market — dissolving border barriers, incompatible standards, and the race-to-the-bottom that fragmented state regulation produced under the Articles of Confederation — while reserving local, non-economic governance to the states closest and most answerable to the communities involved.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction, prosecutorial reach, and rule-setting discretion from state capitals to federal institutions across the entire economic field; moves remedy options out of reach of people harmed by conduct the categorical limits exclude; and moves boundary-litigation costs onto states and private defendants who contest the line.
% ABSENT_VOICES: Ordinary people subject to the resulting rules never sit in the conversation; the line is drawn among judges, legislators, agencies, state attorneys general, and organized interests. Residents of states with weak labor, consumer, or health protections would object that the autonomy delivered to their governments means exposure to those governments' choices — they are represented only indirectly, by officeholders who may prize autonomy over their protection.
% DISAPPEARANCE_RATIONALE: Overnight removal would force immediate renegotiation of every national standard — wages, drug and food safety, environmental floors, financial markets — either collapsing into a fifty-state patchwork with renewed border friction or swinging to unconstrained federal assertion. The national economy runs on this allocation; removing it rearranges the operating system rather than trimming an ornament.
% FOUNDING_PROBLEM: Under the Articles of Confederation, states taxed and blocked one another's trade, coined competing currencies, and left interstate commerce ungovernable; the Philadelphia Convention granted Congress power over commerce among the states to make that trade regular. The modern tripartite-and-limits formulation is the current settlement of the question that grant always carried: how far national power may run before it consumes the local self-government the federal system elsewhere reserves.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era historiography documents the Articles-period trade wars independently of any modern faction; state attorneys general and multi-state coalitions attest, from outside the federal seat, that the reservation of local authority remains consequential enough to spend litigation budgets on; and advocates of the rival readings — who would shrink the paths to cross-border trade or dissolve the categorical limits altogether — confirm that the founding question itself is what divides them. No corroborating source depends on this reading's continuation for its attestation to stand.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).
:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.46 against the standing intermediate allocation assessed by this reading's own lights: the reading endorses the three paths and the limits, and prices the remaining costs — federal subordination of state economic regulation, comprehensive-scheme reach into local instances, and closed federal forums for excluded harms — as the acknowledged price of national-market governance, hence moderate rather than severe. Suppression (0.58) is authored as the raw structural property it is — unscaled by directionality or scope; the engine scales only extraction — and reflects judicial striking of exceedances, federal criminal and civil enforcement, and the absence of any state exit. Theater (0.32) is moderate-low but rising: the security-like core function (real line-drawing in Lopez and Morrison) is genuine, while jurisdictional-element drafting increasingly serves as a fig leaf and recited commerce findings are boilerplate. Accessibility collapse (0.52) is middling because understood alternatives are partly sealed (Article V amendment is practically inaccessible) yet partly live (the governing text admits rival constructions, so judicial reconstruction remains a real channel). Resistance (0.60) is high and persistent: multi-state coalitions, constant private litigation over nexus scope, and organized scholarly movements pressing from both flanks. The temporal series run on one shared seven-point grid (every tracked metric authored at every examined time point, 1995–2026); suppression_requirement is tracked because the story genuinely traces enforcement-capacity change — the judicial machinery for the limits was constructed across 1995–2000, matured through the following decade, and has held roughly stable since, matching the plateau in the series. Extractiveness dips slightly around the 2012 reassertion point before resuming its slow rise. The expected-delta casualty 'conceptual coherence' is deliberately NOT seated as a victim: per the OQ-64 discipline, doctrines collect no rents and bear no extraction, so the manipulability of the limiting principles is carried as vindicated-proposition status plus the boundary-stability and aggregation omegas rather than as a victim-group entry.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat classifications should diverge sharply from the same structural data. The federal seat computes near the beneficiary pole — it collects jurisdiction, prosecutorial reach, and drafting discretion. The state seat computes near-symmetric with a slight net-cost tilt: a confirmed shield over family law, criminal justice, and education, against a ceded economic field and a litigation record in which state coalitions fund challenges and frequently lose. The business seat computes as a net beneficiary whose arbitrage-grade exit damps even its residual compliance burden. The two powerless payer seats — local_harm_victims and intrastate_medical_providers — compute as strong targets: locked in place, unable to relocate their harms or their patients, bearing exclusion-from-forum or sweep-in-destruction directly. Same-level divergence appears between the two institutional agenda setters: Congress experiences the arrangement as drafting friction inside the lines, while the Court experiences it as the legitimacy cost of enforcing an unpopular boundary. Coalition check: the powerless payer seats possess latent coalition capacity — the gender-violence remedy was itself built by a victims' coalition, and patients'-rights organizing is real — but every coalition pathway runs back through the same closed channels (the Court or Article V), which is why measured effective power stays low despite organization.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations drive the derivation: federal_government (agenda setter and named beneficiary) derives near the full-beneficiary end; state_governments carry both declarations and would derive near pure symmetry; interstate_businesses (beneficiary, arbitrage-grade exit) derive near the beneficiary end with damping; the two powerless trapped payer seats derive near the full-target end. One override is authored: organized -> 0.54. Rationale — the dual declaration for states risks mechanical averaging to exact symmetry, but the economic field dominates modern governance, the anti-commandeering resistance failed, and state boundary litigation is funded more often than won; the true relationship sits slightly on the target side of symmetry. The override is keyed to the organized power atom safely because states are the only organized-power agent in this story. Gain-flow receipt: the extraction demonstrably accrues to federal_government — jurisdiction, enforcement reach, and scheme-building discretion land there — while states and businesses receive coordination goods rather than the extraction itself, and the payer seats receive nothing; hence a named seat, not diffuse. Fixing cost is prohibitive relative to benefit for every seat that could fix it: the Court flipping readings would unsettle a century-scale regulatory settlement, and the amendment channel is sealed by supermajority arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing an integrated national economy while preserving local self-government — is live, and the disappearance verdict is world_rearranges, so no mandatrophy declaration attaches and the R5 mismatch consumer should expect no capture/zombie flag (live status x rearranges verdict is the consistent cell). The tangled_rope claim is what prevents both mislabels: reading the arrangement as pure extraction erases the uniformity coordination that interstate commerce cannot function without and that no overnight substitute replaces; reading it as pure coordination erases the recurring, identifiable payers — states ceding the economic field, harm victims locked out of the federal forum, local providers seeing their conduct destroyed as part of someone else's comprehensive scheme. Keeping both halves visible also disciplines the theater signal: the rising jurisdictional-element fig-leaf rate is exactly the kind of proxy-substitution drift the temporal series exists to catch, and it is watched against the boundary-stability and aggregation omegas rather than folded silently into the base score.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This story instantiates only the intermediate_channels reading of the commerce_clause_scope kernel. Which structural facts would change under each sibling reading, and how far does the disagreement reach?',
    'Judicial adoption or durable scholarly entrenchment of a sibling: narrow_originalist deletes the substantial-effects path outright, collapsing the victim set and shrinking federal economic governance to trade facilitation; broad_effects_test dissolves the categorical limits, expanding the target set to every reserved domain whenever cumulative effects can be shown.',
    'Classification flips wholesale: under narrow_originalist this becomes a thin facilitation arrangement with negligible extraction; under broad_effects_test it becomes a near-plenary federal assertion with high extraction and a vastly enlarged target population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Kernel membership: one of three rival readings; sibling adoption changes beneficiaries, victims, and computed type.').

omega_variable(
    economic_noneconomic_boundary_stability,
    'Is the economic/non-economic distinction that carries all three limiting principles a stable feature of human activity, or a manipulable label that statutory drafting can attach or detach at will?',
    'Systematic coding of post-1995 statutes'' jurisdictional elements against courts'' acceptance patterns: whether interstate-nexus clauses track the real interstate character of the regulated conduct or serve as fig leaves grafted onto bills whose objects are local.',
    'If the boundary is chiefly manipulable, the limiting principles are performative and the arrangement degrades in operation toward the broad reading''s reach — rising theater and extraction, drift toward a harder type. If stable, the limits do real work and the measured moderation holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_boundary_stability, empirical, 'Stability of the economic/non-economic line on which the jurisdictional-element, aggregation, and anti-attenuation limits all depend.').

omega_variable(
    aggregation_swallow_risk,
    'Does aggregate-effects reasoning, validated for comprehensive economic schemes, make the substantial-effects path effectively unlimited for economic activity — leaving the categorical limits guarding only the non-economic residue?',
    'Counterfactual probes: statutes regulating plainly local economic conduct justified purely through aggregated nationwide effects, and the courts'' dispositions of them; compare dispositions where no comprehensive scheme exists.',
    'If aggregation swallows the economic field, this reading''s practical difference from the broad reading narrows to the non-economic remainder and measured federal extraction trends upward; if aggregation stays bounded, the middle reading holds distinct ground between its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_swallow_risk, empirical, 'Whether aggregation logic bounds or unbounds the third authorized path.').

omega_variable(
    state_shield_substantiveness,
    'Do the categorical limits deliver state governments real, usable policy space over local matters, or mainly relocate the conflict into litigation the states fund and more often than not lose?',
    'Compare the volume and durability of state policy divergence inside the reserved (non-economic) versus the ceded (economic) spheres since 1995, and tally state-initiated boundary challenges against their outcomes.',
    'If the shield is largely symbolic, the declared beneficiary position of the states is illusory and their effective directional pull sits nearer the target end than the dual declaration suggests; if substantive, the dual positioning is genuine and the near-symmetry estimate stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_shield_substantiveness, empirical, 'Substantiveness of the state-autonomy shield the categorical limits provide.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 31).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.16).
narrative_ontology:measurement(comm_tr_t5, commerce_clause_scope__intermediate_channels, theater_ratio, 5, 0.2).
narrative_ontology:measurement(comm_tr_t11, commerce_clause_scope__intermediate_channels, theater_ratio, 11, 0.24).
narrative_ontology:measurement(comm_tr_t17, commerce_clause_scope__intermediate_channels, theater_ratio, 17, 0.27).
narrative_ontology:measurement(comm_tr_t23, commerce_clause_scope__intermediate_channels, theater_ratio, 23, 0.3).
narrative_ontology:measurement(comm_tr_t28, commerce_clause_scope__intermediate_channels, theater_ratio, 28, 0.31).
narrative_ontology:measurement(comm_tr_t31, commerce_clause_scope__intermediate_channels, theater_ratio, 31, 0.32).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(comm_be_t5, commerce_clause_scope__intermediate_channels, base_extractiveness, 5, 0.39).
narrative_ontology:measurement(comm_be_t11, commerce_clause_scope__intermediate_channels, base_extractiveness, 11, 0.42).
narrative_ontology:measurement(comm_be_t17, commerce_clause_scope__intermediate_channels, base_extractiveness, 17, 0.44).
narrative_ontology:measurement(comm_be_t23, commerce_clause_scope__intermediate_channels, base_extractiveness, 23, 0.43).
narrative_ontology:measurement(comm_be_t28, commerce_clause_scope__intermediate_channels, base_extractiveness, 28, 0.45).
narrative_ontology:measurement(comm_be_t31, commerce_clause_scope__intermediate_channels, base_extractiveness, 31, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__intermediate_channels, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(comm_su_t5, commerce_clause_scope__intermediate_channels, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(comm_su_t11, commerce_clause_scope__intermediate_channels, suppression_requirement, 11, 0.57).
narrative_ontology:measurement(comm_su_t17, commerce_clause_scope__intermediate_channels, suppression_requirement, 17, 0.57).
narrative_ontology:measurement(comm_su_t23, commerce_clause_scope__intermediate_channels, suppression_requirement, 23, 0.56).
narrative_ontology:measurement(comm_su_t28, commerce_clause_scope__intermediate_channels, suppression_requirement, 28, 0.57).
narrative_ontology:measurement(comm_su_t31, commerce_clause_scope__intermediate_channels, suppression_requirement, 31, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Commerce Clause's scope' conflates at least three structurally distinct allocations of federal power with different extraction profiles, victim sets, and stability conditions; this file authors the intermediate_channels member of that family. Links run to commerce_clause_scope__narrow_originalist and commerce_clause_scope__broad_effects_test. Cross-reading texture differs per sibling: Lopez-era limit enforcement lent the originalist program its modern legitimacy, while Raich-era aggregation practice handed the broad-effects program its defensive limiting principle (containment via comprehensive-scheme membership). Each sibling file should carry its own epsilon and link back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, organized, 0.54).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
