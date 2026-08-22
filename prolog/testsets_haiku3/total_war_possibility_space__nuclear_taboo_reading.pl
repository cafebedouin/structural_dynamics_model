% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear War Taboo via Norm Entrenchment
 *   domain: international_relations/institutional_history
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'total_war_possibility_space': the nuclear taboo reading. The constraint
 *   asserts that total war became normatively prohibited through constructed
 *   social understanding independent of material capability — that is, the
 *   taboo is constitutive, not derivative from deterrence equilibrium. States
 *   retain the material capability to wage total war; the constraint is a
 *   norm that makes that choice illegitimate within the community's
 *   self-understanding. This reading predicts: (1) the taboo generates active
 *   norm-enforcement mechanisms (non-proliferation regimes, no-first-use
 *   pledges); (2) it weakens if key norm entrepreneurs exit; (3) non-nuclear
 *   threshold states face asymmetrically higher enforcement pressure than
 *   grandfathered nuclear powers; (4) the taboo's persistence depends on
 *   continuous discursive reproduction, not merely on material vulnerability.
 *   Sibling readings treat the same kernel differently: the
 *   deterrence_equilibrium reading sees total war as strategically reachable
 *   but materially deterred (the taboo is epiphenomenal); the
 *   space_contraction reading sees nuclear weapons as having removed total
 *   war from strategic thinkability altogether (a cognitive constraint, not a
 *   normative one). All three readings share the referent — the standing
 *   arrangement of human capability and state behavior after 1945 — but
 *   author different ε values and structural mechanisms.
 *
 * KEY AGENTS:
 *   - Non-proliferation epistemic community (IAEA, NPT signatories, norm entrepreneurs) — maintains and reproduces the taboo through institutional practice and discourse
 *   - Nuclear weapons states (US, Russia, China, UK, France) — possess capability, enforce taboo asymmetrically, benefit from norm without needing constant use threats
 *   - Extended-deterrence beneficiaries (NATO allies, Japan, South Korea) — gain security from taboo's existence, constrained exit options
 *   - Threshold states (Iran, North Korea, Pakistan) — face asymmetric enforcement costs when pursuing weapons programs
 *   - Norm-skeptical strategists — maintain that taboo is derivative from material deterrence, excluded from public norm-setting discourse
 *   - Analytical observers — test whether taboo is causally constitutive or merely describes outcomes material incentives produce
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.31).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.58).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear War Taboo via Norm Entrenchment").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '16276eaa-e7cb-4966-b608-02da38fd01f9').
narrative_ontology:cs_kernel_codification('16276eaa-e7cb-4966-b608-02da38fd01f9', fixed_text).
narrative_ontology:cs_authority_grounding('16276eaa-e7cb-4966-b608-02da38fd01f9', lineage).
narrative_ontology:cs_interpretation_layer_present('16276eaa-e7cb-4966-b608-02da38fd01f9').
narrative_ontology:cs_reading_relation('16276eaa-e7cb-4966-b608-02da38fd01f9', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('16276eaa-e7cb-4966-b608-02da38fd01f9', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('16276eaa-e7cb-4966-b608-02da38fd01f9', foundational, nuclear_war_normatively_foreclosed).
narrative_ontology:cs_axiom_status(nuclear_war_normatively_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('16276eaa-e7cb-4966-b608-02da38fd01f9', nuclear_war_normatively_foreclosed, deontological).
narrative_ontology:cs_axiom('16276eaa-e7cb-4966-b608-02da38fd01f9', foundational, taboo_constitutive_of_possibility).
narrative_ontology:cs_axiom_status(taboo_constitutive_of_possibility, holdable).
narrative_ontology:cs_axiom_grounding('16276eaa-e7cb-4966-b608-02da38fd01f9', taboo_constitutive_of_possibility, conventional).
narrative_ontology:cs_axiom('16276eaa-e7cb-4966-b608-02da38fd01f9', secondary, institutional_norm_maintenance_efficacious).
narrative_ontology:cs_axiom_status(institutional_norm_maintenance_efficacious, overridden).
narrative_ontology:cs_axiom_grounding('16276eaa-e7cb-4966-b608-02da38fd01f9', institutional_norm_maintenance_efficacious, empirically_contingent).
narrative_ontology:cs_reference_frame('16276eaa-e7cb-4966-b608-02da38fd01f9', nuclear_war_categorically_impermissible).
narrative_ontology:cs_drift_state('16276eaa-e7cb-4966-b608-02da38fd01f9', contemporary_threshold_proliferation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('16276eaa-e7cb-4966-b608-02da38fd01f9', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_epistemic_community).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_beneficiaries).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapons_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, post_conflict_reconstruction_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, threshold_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academics, diplomats, and norm entrepreneurs (NPT signatories, IAEA inspectors, research institutions) who maintain and reproduce the taboo through discourse, treaty interpretation, and institutional practice. They define what counts as a violation, interpret threshold breaches, and organize enforcement through inspection regimes and diplomatic isolation. The taboo's persistence depends on their continued framing work.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_epistemic_community, agenda_setter,
    institutional, generational, mobile, global).

% Possess the material capability to wage total war but are bound by the taboo they helped construct and now defend. They benefit from the norm's asymmetric application: their arsenals are grandfathered under non-proliferation frameworks while new entrants face severe sanctions. They enforce the taboo selectively against threshold-crossers (Iran, North Korea) while maintaining first-strike doctrines of their own. The norm sustains their strategic advantage without requiring constant active use threats.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapons_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapons_states, agenda_setter).

% Non-nuclear states under security umbrellas (NATO allies, Japan, South Korea) who benefit from the taboo's existence: they are guaranteed against existential threats by the norm's entrenchment rather than by their own material capability. The taboo makes the nuclear umbrella's protection both more credible (bounded by the norm) and more secure (no peer escalation expected). Exit would require independent deterrence capacity they do not possess.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_beneficiaries, beneficiary,
    powerful, generational, constrained, global).

% Regional powers (Iran, North Korea, Pakistan, Israel) who face the taboo's enforcement: their pursuit of weapons programs triggers international sanctions, isolation, and military intervention threats. The taboo's selectivity — it is enforced against them while grandfathered states retain arsenals — makes it economically and diplomatically costly to cross. They bear the suppression cost; the norm's beneficiaries do not.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, threshold_states, payer,
    powerful, biographical, constrained, regional).

% Military planners and realist theorists who maintain that the taboo is rhetoric covering material deterrence. They argue total war remains strategically thinkable and that the norm is epiphenomenal — a narrative constructed after-the-fact to explain what material vulnerability already achieved. They are heard in classified strategic planning but excluded from the public norm-setting conversation; their voice remains marginal in the institutional discourse that reproduces the taboo.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_skeptical_strategists, excluded,
    powerful, biographical, constrained, global).

% States recovering from conventional conflicts (Germany post-1945, Japan, Korea) who benefit from the taboo's extension: the norm prevents total war escalation and enables reconstruction within a bounded conflict framework. They gain sovereignty within a stable international order because existential threats remain formally off-limits. Their recovery is contingent on the taboo holding.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, post_conflict_reconstruction_states, beneficiary,
    moderate, biographical, mobile, regional).

% Academic observers (game theorists, strategic analysts) who analyze whether the taboo is constitutive or derivative. They test whether norms causally constrain behavior or merely describe outcomes that material incentives already produce. Their analytical stance permits observation of the constraint without commitment to the reading.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, rational_choice_realists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates shared normative understanding that total war is civilizationally illegitimate, solving the commitment problem that pure material deterrence cannot: mutual vulnerability is necessary but insufficient to prevent escalation in crisis. The taboo provides an additional institutional constraint that makes non-use credible as a permanent commitment rather than as a temporary strategy contingent on material equilibrium. Enables stability under uncertainty and reduces rational-actor pressure toward first-strike calculation.
% TRANSFER_FUNCTION: Transfers strategic advantage from threshold states to early-adopter nuclear powers: the norm permits existing arsenals while prohibiting new ones, grandfathering current powers while imposing disproportionate cost on aspirants. Moves authority over nuclear strategy from individual military calculation to international epistemic community (IAEA, NPT architects, norm entrepreneurs) who set the boundaries of legitimate state behavior. Transfers security benefit to extended-deterrence states (who gain protection from the taboo's hold) and cost to threshold states (who face enforcement when acquiring capability). Transfers institutional authority from military planners (whose strategic logic might justify use) to diplomats and international lawyers (whose norm logic emphasizes prohibition).
% ABSENT_VOICES: Threshold states that would contest the norm's legitimacy are formally included in treaty regimes but have minimal voice in norm-setting (they are governed by the NPT but do not set its terms). Military strategists who maintain the taboo is epiphenomenal to material deterrence are institutionally marginalized in diplomatic discourse (their views are classified or confined to military academies, not to treaty negotiations). Populations in nuclear-armed states, who would bear the cost of deterrence failure or of normative escalation, are excluded from strategic decision-making. Future generations for whom the taboo's durability under changed material circumstances is existentially urgent are not represented. Some regional powers (Israel, Pakistan) operate outside the NPT framework and contest its legitimacy, but their voice is excluded from the official norm-setting conversation (treated as violators rather than as participants).
% DISAPPEARANCE_RATIONALE: If the taboo dissolved and international understanding that total war is illegitimate evaporated, strategic calculations would shift immediately and dramatically. The non-proliferation regime's legal framework would collapse and threshold states would pursue weapons acquisition without enforcement cost. Existing nuclear powers would face pressure to justify arsenals under new legitimacy rules. Extended-deterrence relationships would undergo credibility crises (would the umbrella holder really violate the newly-defunct taboo in actual use?). Arms races would resume with weaker commitment devices — the deterrence equilibrium would need to be renegotiated from bare material vulnerability without institutional lock-in. Regional conflicts (Middle East, South Asia, East Asia) would face existential risk of escalation to nuclear use that is currently suppressed by norm. The international order's stability structure depends substantially on the taboo persisting; its removal would trigger rapid re-equilibration toward higher-variance and higher-risk strategic behaviors.
% FOUNDING_PROBLEM: After 1945, humanity possessed the material capability to wage total war with nuclear weapons, creating species-level risk. Early nuclear doctrine (1950s flexible response, graduated escalation, tactical nukes) treated nuclear weapons as usable tools in escalation ladders. The founding problem was: how to prevent rational strategists, operating under uncertainty and pressure in crisis situations, from calculating that nuclear use is justified in extreme circumstances, when the mutual vulnerability that prevents escalation is assumed but might fail? How to create commitment to non-use that is stable across multiple generations and across regime changes?
% FOUNDING_PROBLEM_CORROBORATION: Non-proliferation architects, IAEA officials, and norm entrepreneurs attest that the founding problem required institutional solution because material deterrence alone provides insufficient credible commitment. They cite declassified strategic doctrine from 1950s–1960s showing that use was seriously considered as a military option, and argue that institutional entrenchment of taboos (NPT, test-ban treaty, no-first-use pledges) shifted the calculation. Military strategists and game theorists attest the problem was solved by material factors (the physics of mutual vulnerability created rational non-use) and argue the institutional regime is epiphenomenal theater. Declassified Cold War documents show both tracks operating: (1) real pressure toward use in multiple crises (Cuba, Berlin, Korean escalation in 1953, India-Pakistan wars) where strategic logic permitted use, and (2) explicit norm-construction work by epistemic communities (arms-control negotiators, diplomatic conferences, scholarly networks building consensus on non-use). The corroboration is mixed: evidence supports both readings simultaneously. No independent external authority (neither the epistemic community nor the strategists) holds monopoly on truth. The founding problem remains live and contested because the causal mechanism (norm vs. material deterrence) remains empirically indeterminate 80 years later.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.31 rising to 0.31 over 80 years with peak at 1990 of 0.32) reflects moderate asymmetric advantage: nuclear-armed states benefit from possessing weapons under a norm prohibiting their use, while threshold states face disproportionate enforcement costs. This is not pure extraction — the non-proliferation regime delivers genuine coordination value (reduced proliferation, lower war frequency) — but it does concentrate advantage in the hands of early entrants who are grandfathered under non-proliferation frameworks. Suppression rises from 0.35 to 0.58 (peak 0.62 in 2006 during Iran and North Korea escalations) reflecting increasing enforcement activity as threshold states challenged the taboo: sanctions regimes, military threats, inspection missions, and diplomatic isolation intensified through the nuclear standoffs of the 2000s. Theater ratio rises from 0.08 to 0.22, indicating growing performative maintenance: norm entrepreneurs increasingly engage in rhetorical defense of the taboo (treaties, summit declarations, disarmament rhetoric) as actual enforcement costs mount and threshold-state challenges proliferate. The leveled coercion grid shows asymmetric suppression: structural-level suppression rises sharply (0.22 to 0.62) as non-proliferation institutions strengthen; organizational and class-level suppression remains moderate, reflecting selective enforcement (some threshold states are tolerated or accommodated; others are isolated). Individual resistance falls (0.55 to 0.41) as the taboo becomes internalized in state identity and strategic culture — citizens and lower-level military officers are socialized into non-use norms. The grid's temporal progression shows a norm-entrenchment pattern: alternatives collapse (accessibility_collapse rises), stakes of violation rise (stakes_inflation increases), organized suppression machinery is constructed (suppression rises), but structural-level resistance falls as the norm becomes habitual rather than contested.
 *
 * PERSPECTIVAL GAP:
 *   The taboo reading predicts radically different type classifications across seats. From the non-proliferation community's seat, the constraint is rope (genuine coordination solving a real collective-action problem: preventing proliferation and reducing war frequency). From the threshold state's seat, the constraint is tangled_rope or snare (asymmetric enforcement, selective benefits, active suppression of their own aspirations). From the nuclear weapons state's seat in a liberal democracy, the constraint might appear as rope (security through stability); from the same state's military planning seat, it might appear as snare (constraining legitimate deterrence options). From the population of a threshold state in a region facing an adversary with nuclear weapons, the constraint is extractive (denies them the deterrence capacity their adversary possesses). The engine computes these seat-by-seat classifications from the structural data (power atoms, exit options, beneficiary/victim status); the authored claim does NOT adjudicate them. This divergence is the central measurement the taboo-reading hypothesis enables: if the same constraint computes as rope in one seat and snare in another, that gap tells us that extraction is real and asymmetrically distributed, not that classification is merely observer-dependent.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declaration: The non-proliferation epistemic community, extended deterrence beneficiaries, and nuclear weapons states all benefit from the norm's existence — they derive authority, security, or strategic advantage from it. This benefits set is uneven: the epistemic community derives institutional authority and funding; nuclear weapons states derive asymmetric advantage (grandfathered arsenals); extended deterrence beneficiaries derive existential protection they could not otherwise afford. Victim declaration: None are declared because the constraint, read as taboo, does not have direct victims in the sense of targeted populations bearing costs (no population is the constraint's target in the way a debt-trap victim is targeted). Rather, it has paying seats: threshold states pay enforcement costs, norm-skeptical strategists pay institutional marginalization, future generations pay the risk of norm erosion. The constraint is structurally beneficial overall (war has decreased, proliferation has slowed) but asymmetrically beneficial — the gains concentrate in early-adopter nuclear powers and their allies. Directionality overrides: None are necessary; the structural derivation (beneficiaries with institutional/powerful power atoms = low d; threshold states with constrained exit = high d) produces coherent directionality values without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear escalation in a world of mutual vulnerability) was alive and urgent in 1945–1990 (Cold War era, repeated crises, proliferation risks). It remains contested at 2025: nuclear-armed states argue the problem is live and the taboo is essential; threshold states argue the problem is that the taboo is selectively enforced and unfairly restricts their security options; realists argue the problem was solved by material deterrence, not norms, and the taboo is post-hoc narrative. The constraint does not exhibit mandatrophy (a dead founding problem persisting as zombie inertia) in the classical sense because the founding problem remains institutionally disputed. However, the rising theater_ratio (0.08 to 0.22) and suppression_requirement (0.35 to 0.58 peaking at 0.62) suggest increasing cost of maintenance: the norm requires more active enforcement and more rhetorical defense as material threats to its persistence mount (threshold proliferation, erosion of non-proliferation commitment by rising powers, strategic doctrine shifts toward lower-yield tactical nuclear weapons). The constraint is not yet a piton (it generates real coordination value and real resistance to its violation) but it is showing signs of strain that could lead to piton-like dynamics if norm entrepreneurs disengage or if a threshold state successfully breaks the taboo without severe consequences. The measurement grid shows this strain: organizational-level suppression is lower than structural (the treaty bureaucracies are strong, but state-level compliance is increasingly costly and contested), and individual-level resistance is falling (lower levels of society are becoming desocialized from the no-use norm as nuclear doctrine shifts). If this trend continues, the constraint could transition from rope (genuine coordination, shared understanding) toward tangled_rope (asymmetric extraction riding on coordination) or even snare (pure extraction of threshold-state compliance without reciprocal benefit). The mandate is not dead; it is hardening against erosion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    norm_vs_material_causation,
    'Is the 70-year absence of nuclear use causally driven by the taboo''s normative force, or is it merely correlated with material deterrence equilibrium that would produce the same outcome without the norm?',
    'Counterfactual analysis: (1) if material deterrence alone (mutual vulnerability) can explain non-use in every case where it was invoked, the norm is epiphenomenal; (2) if cases exist where material deterrence predicted use but the norm prevented it (or vice versa), causation is real; (3) experimental or quasi-experimental evidence from arms-control negotiations: do norm entrepreneurs'' speeches and treaty language measurably change state behavior beyond what material incentives would predict?',
    'If material causation dominates, the constraint should be reclassified as derivative from deterrence_equilibrium_reading — it becomes a snare (asymmetric advantage leveraging a material fact) rather than a rope (genuine coordination solving a collective-action problem via norms). If norm causation dominates, the taboo reading stands as rope (norms enable coordination that material factors alone could not). If both causations are active (a genuinely hybrid case), the constraint is tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(norm_vs_material_causation, empirical, 'Whether the taboo is causally constitutive or merely describes outcomes material vulnerability already ensures.').

omega_variable(
    norm_entrepreneur_exit_fragility,
    'How brittle is the taboo to withdrawal of key norm entrepreneurs (states, epistemic community, treaty institutions)? Would the taboo persist if the non-proliferation regime weakened or if major powers deprioritized norm maintenance?',
    'Historical natural experiment: (1) Cold War institutions that deprioritized non-use norms in the 1950s–1960s (flexible response doctrine, tactical nuclear weapons) — did use remain suppressed, or did escalation doctrine shift expectations? (2) Post-Cold War periods of reduced non-proliferation enforcement (1990s, post-9/11 shift to counter-terrorism priorities) — did proliferation accelerate when enforcement weakened? (3) Strategic doctrine shifts toward ''usable'' nuclear weapons (low-yield, cyber integration) — do they track norm erosion or represent independent military innovation?',
    'If the taboo persists despite norm-entrepreneur exit, it has become constitutive of state identity and strategic culture — genuinely hard-wired, not performatively maintained. If it erodes immediately when enforcement weakens, it is theater and surveillance-dependent, vulnerable to cascading collapse. Fragility assessment changes the constraint''s type: brittle taboos are snares (suppression-dependent), resilient taboos are ropes (identity-constituted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_exit_fragility, empirical, 'Whether the taboo''s persistence depends on continuous norm-entrepreneur maintenance or has become internalized institutional fact.').

omega_variable(
    threshold_state_asymmetry_justification,
    'Is the selective enforcement of non-proliferation norms against threshold states (Iran, North Korea) while grandfathering existing powers structurally justified by security logic, or is it pure hegemonic extraction using the norm as cover?',
    '(1) Structural stability analysis: does allowing new powers to acquire arsenals genuinely destabilize regional security, or would multipolarity reduce escalation risk (through distribution of deterrence)? (2) Counterfactual parity test: if all states had equivalent arsenals, would strategic equilibrium be more or less stable than today? (3) Threshold-state perspective: are their objections to the regime primarily about fairness (hypocrisy of existing powers), capability (they are denied deterrence), or both?',
    'If selective enforcement is justified by genuine security logic (new entrants would escalate stability risk), the constraint is rope with legitimate asymmetry — the cost to threshold states is purchase of global stability. If it is unjustified extraction, the constraint is snare — threshold states pay for a system designed to advantage existing powers. If threshold states could build stable multipolar deterrence without increasing war risk, the regime''s claim to security benefit collapses and it becomes pure hegemonic control dressed as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_asymmetry_justification, conceptual, 'Whether asymmetric non-proliferation enforcement protects genuine collective security or serves hegemonic extraction.').

omega_variable(
    reading_foreclosure_test,
    'Do the three kernel readings (deterrence_equilibrium, nuclear_taboo, space_contraction) logically foreclose each other, or do they genuinely coexist as different frames on the same phenomenon?',
    '(1) Logical test: if material deterrence fully explains non-use, is the taboo reading logically foreclosed? Or can both be true (the material factor prevents escalation AND the norm prevents even consideration of it)? (2) Empirical scope test: each reading makes different predictions about threshold conditions — at what point would each predict abandonment of the constraint? (3) Framework commensurability test: can a single strategic decision-making process simultaneously employ deterrence logic (calculating costs) AND taboo logic (treating options as unthinkable)?',
    'If the readings foreclose each other, the kernel represents a genuine contested claim about causation — only one can be true and evidence should adjudicate. If they coexist, the kernel represents a framework-choice, not an empirical dispute — different institutional contexts invoke different readings without logical contradiction. This determines whether the corpus should track the readings as competing hypotheses or as coexisting institutional frames.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether sibling readings of the total_war_possibility_space kernel are logically incompatible or genuinely coexistent institutional frames.').

omega_variable(
    norm_internalization_vs_suppression,
    'To what extent is observed non-use of nuclear weapons due to internalized norms (states have accepted the taboo as legitimate) versus external suppression (states are forced to comply by enforcement machinery against their preference)?',
    '(1) Elite preference revelation: do strategic planners in closed settings describe non-use as normatively binding (internalized) or as externally constrained (suppressed)? (2) Decoupling test: if enforcement mechanisms weakened without norm entrepreneurs noticing, would compliance rates drop? (3) Generational analysis: has the taboo become stronger in younger cohorts of policymakers (evidence of internalization across time), or has commitment varied with enforcement intensity (evidence of suppression)?',
    'High internalization means the constraint is an identity-constituted rope: states have become the kind of actors that do not use nuclear weapons. High suppression means the constraint is enforcement-dependent tangled_rope or snare: compliance is coerced, not voluntary. The distinction determines fragility (internalized constraints survive norm-entrepreneur exit; suppressed constraints collapse). It also determines the measurement of theater_ratio: high theater with low suppression suggests true norm commitment; high theater with high suppression suggests maintenance of a facade against internal pressure to defect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_internalization_vs_suppression, empirical, 'Whether observed non-use is internalized as legitimate (rope) or externally forced (snare/tangled_rope).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1962, 0.12).
narrative_ontology:measurement(tota_tr_t1974, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1974, 0.16).
narrative_ontology:measurement(tota_tr_t1990, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1990, 0.21).
narrative_ontology:measurement(tota_tr_t2006, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2006, 0.25).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1962, 0.25).
narrative_ontology:measurement(tota_be_t1974, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1974, 0.28).
narrative_ontology:measurement(tota_be_t1990, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(tota_be_t2006, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2006, 0.32).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2025, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1962, 0.45).
narrative_ontology:measurement(tota_su_t1974, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1974, 0.52).
narrative_ontology:measurement(tota_su_t1990, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(tota_su_t2006, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2006, 0.62).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2025, 0.58).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2025
narrative_ontology:measurement(tota_grid_01, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(class), 1945, 0.31).
narrative_ontology:measurement(tota_grid_02, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(class), 2025, 0.72).
narrative_ontology:measurement(tota_grid_03, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(individual), 1945, 0.28).
narrative_ontology:measurement(tota_grid_04, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(individual), 2025, 0.65).
narrative_ontology:measurement(tota_grid_05, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(organizational), 1945, 0.38).
narrative_ontology:measurement(tota_grid_06, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(organizational), 2025, 0.68).
narrative_ontology:measurement(tota_grid_07, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(structural), 1945, 0.42).
narrative_ontology:measurement(tota_grid_08, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(structural), 2025, 0.71).
narrative_ontology:measurement(tota_grid_09, total_war_possibility_space__nuclear_taboo_reading, resistance(class), 1945, 0.61).
narrative_ontology:measurement(tota_grid_10, total_war_possibility_space__nuclear_taboo_reading, resistance(class), 2025, 0.35).
narrative_ontology:measurement(tota_grid_11, total_war_possibility_space__nuclear_taboo_reading, resistance(individual), 1945, 0.55).
narrative_ontology:measurement(tota_grid_12, total_war_possibility_space__nuclear_taboo_reading, resistance(individual), 2025, 0.41).
narrative_ontology:measurement(tota_grid_13, total_war_possibility_space__nuclear_taboo_reading, resistance(organizational), 1945, 0.48).
narrative_ontology:measurement(tota_grid_14, total_war_possibility_space__nuclear_taboo_reading, resistance(organizational), 2025, 0.42).
narrative_ontology:measurement(tota_grid_15, total_war_possibility_space__nuclear_taboo_reading, resistance(structural), 1945, 0.52).
narrative_ontology:measurement(tota_grid_16, total_war_possibility_space__nuclear_taboo_reading, resistance(structural), 2025, 0.38).
narrative_ontology:measurement(tota_grid_17, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(class), 1945, 0.72).
narrative_ontology:measurement(tota_grid_18, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(class), 2025, 0.85).
narrative_ontology:measurement(tota_grid_19, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(individual), 1945, 0.61).
narrative_ontology:measurement(tota_grid_20, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(individual), 2025, 0.76).
narrative_ontology:measurement(tota_grid_21, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(organizational), 1945, 0.55).
narrative_ontology:measurement(tota_grid_22, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(organizational), 2025, 0.78).
narrative_ontology:measurement(tota_grid_23, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(structural), 1945, 0.68).
narrative_ontology:measurement(tota_grid_24, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(structural), 2025, 0.81).
narrative_ontology:measurement(tota_grid_25, total_war_possibility_space__nuclear_taboo_reading, suppression(class), 1945, 0.28).
narrative_ontology:measurement(tota_grid_26, total_war_possibility_space__nuclear_taboo_reading, suppression(class), 2025, 0.54).
narrative_ontology:measurement(tota_grid_27, total_war_possibility_space__nuclear_taboo_reading, suppression(individual), 1945, 0.25).
narrative_ontology:measurement(tota_grid_28, total_war_possibility_space__nuclear_taboo_reading, suppression(individual), 2025, 0.48).
narrative_ontology:measurement(tota_grid_29, total_war_possibility_space__nuclear_taboo_reading, suppression(organizational), 1945, 0.31).
narrative_ontology:measurement(tota_grid_30, total_war_possibility_space__nuclear_taboo_reading, suppression(organizational), 2025, 0.58).
narrative_ontology:measurement(tota_grid_31, total_war_possibility_space__nuclear_taboo_reading, suppression(structural), 1945, 0.22).
narrative_ontology:measurement(tota_grid_32, total_war_possibility_space__nuclear_taboo_reading, suppression(structural), 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel split. The deterrence_equilibrium_reading treats the same nuclear non-use phenomenon as derivative from material vulnerability (high ε for the taboo itself, low for deterrence). The space_contraction_reading treats it as cognitive impossibility (high accessibility_collapse, near-zero resistance from strategic planners). The three readings share the referent (70 years of non-use) but assign causation differently. The taboo reading's falsification condition: if norm entrepreneurs exit (epistemic community deprioritizes non-proliferation work), enforcement suppression collapses (threshold states face no cost to proliferate), and use rates rise without material incentive change, the reading is false. The deterrence reading's falsification condition: if mutual vulnerability is removed (one side achieves decisive first-strike capability), deterrence fails, and use occurs regardless of taboo strength. The space_contraction reading's falsification condition: if strategic planners explicitly recalculate total war as thinkable (doctrine shifts, strategic documents treat it as option), the constraint is cognitive, not transcendent. Each reading can be tested against the other through evidence of which mechanism is active in actual strategic behavior.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
