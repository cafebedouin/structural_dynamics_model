% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Absolute Non-Intervention Sovereignty Doctrine
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   The absolute non-intervention reading of the Westphalia sovereignty
 *   kernel posits that territorial inviolability is categorical—external
 *   interference in state affairs is per se illegitimate regardless of
 *   internal conduct, atrocity, or regime type. This constraint protects
 *   state elites from accountability and external pressure by erecting a
 *   legal barrier against humanitarian intervention. It benefits established
 *   powers (who use it selectively) and authoritarian regimes (who invoke it
 *   absolutely), while victimizing populations under atrocity who are
 *   declared to be outside the scope of legitimate international concern
 *   precisely because they are internal. The constraint's emergence from the
 *   post-WWII decolonization settlement was justified as prevention of
 *   great-power conquest under humanitarian cover; that founding problem is
 *   substantially dead, replaced by a different pathology: the doctrine now
 *   functions as legal shield for domestic repression. The coercion grid
 *   shows marked level divergence: at the structural level (international
 *   legal system), alternative intervention frameworks remain available and
 *   resistance is moderate; at the individual level (persecuted persons),
 *   accessibility collapses near completely and resistance is minimal,
 *   revealing the constraint's asymmetric cost distribution.
 *
 * KEY AGENTS:
 *   - authoritarian_state_elites: agenda-setter at institutional power, trapped exit, generational horizon — control the state and deploy the doctrine as shield against external pressure
 *   - populations_under_atrocity: payer at powerless level, trapped exit, immediate horizon — structurally excluded from international legal protection, declared an internal matter
 *   - established_great_powers: beneficiary at institutional power, arbitrage exit, generational horizon — invoke non-intervention selectively, maintain right to intervene through other frames
 *   - international_humanitarian_advocates: excluded by definition, organized level, constrained exit — would challenge doctrine on atrocity-protection grounds but are defined out of the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.82).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.71).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.82).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, snare).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Absolute Non-Intervention Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '7e4e5e8b-7f42-4159-915b-11c38d49323a').
narrative_ontology:cs_kernel_codification('7e4e5e8b-7f42-4159-915b-11c38d49323a', formalized).
narrative_ontology:cs_authority_grounding('7e4e5e8b-7f42-4159-915b-11c38d49323a', lineage).
narrative_ontology:cs_interpretation_layer_present('7e4e5e8b-7f42-4159-915b-11c38d49323a').
narrative_ontology:cs_reading_relation('7e4e5e8b-7f42-4159-915b-11c38d49323a', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_reading_relation('7e4e5e8b-7f42-4159-915b-11c38d49323a', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('7e4e5e8b-7f42-4159-915b-11c38d49323a', foundational, absolute_territorial_inviolability).
narrative_ontology:cs_axiom_status(absolute_territorial_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('7e4e5e8b-7f42-4159-915b-11c38d49323a', absolute_territorial_inviolability, deontological).
narrative_ontology:cs_axiom('7e4e5e8b-7f42-4159-915b-11c38d49323a', secondary, internal_affairs_immunity_doctrine).
narrative_ontology:cs_axiom_status(internal_affairs_immunity_doctrine, overridden).
narrative_ontology:cs_axiom_grounding('7e4e5e8b-7f42-4159-915b-11c38d49323a', internal_affairs_immunity_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('7e4e5e8b-7f42-4159-915b-11c38d49323a', categorical_territorial_inviolability).
narrative_ontology:cs_drift_state('7e4e5e8b-7f42-4159-915b-11c38d49323a', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7e4e5e8b-7f42-4159-915b-11c38d49323a', '2026-06-11T14:32:18Z').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, established_great_powers).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, territorial_status_quo_defenders).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_atrocity).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, persecuted_minorities).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, internal_dissidents).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, state_territorial_monopoly).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, formal_equality_of_sovereigns).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, non_interference_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the state apparatus and enforce the doctrine domestically. They deploy absolute non-intervention as legal shield against external pressure when conducting repression, ethnic cleansing, or mass atrocity. They set the agenda by claiming the doctrine's protection and defending it diplomatically. Their extraction is the protected space to commit internal harms without external legal grounds for intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites, agenda_setter,
    institutional, generational, trapped, national).

% Bear the direct costs of the doctrine: they are excluded from international protection on the grounds that their suffering is an internal matter. They cannot exit the territory (trapped by geography, violence, or closure); cannot appeal to external authority; and are denied standing in the framework that governs their fate. Their exclusion is the doctrine's structural requirement.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_atrocity, payer,
    powerless, immediate, trapped, local).

% Benefit from the doctrine by claiming its protection when it suits them (invoking sovereignty against ICC accountability, sanctions, or humanitarian intervention) while maintaining the right to intervene outside the doctrine's frame through security alliances, economic coercion, or humanitarian exception rhetoric. They preserve their own enforcement machinery while limiting others' intervention rights through the doctrine's formal legitimacy.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, established_great_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Include regional hegemonies and colonial successor states that benefit from the doctrine's barrier to redrawing borders or challenging their territorial holdings. The doctrine insulates them from secessionist intervention, indigenous claims, or external pressure to cede territory. They are distinguished from great powers by regional rather than global scope but share the extraction benefit.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, territorial_status_quo_defenders, beneficiary,
    institutional, generational, arbitrage, global).

% Suffer targeted persecution justified as internal administrative matter. Identity-locked because exit requires either accepting erasure of group identity or fleeing the territory; neither removes the structural exclusion from international legal standing. They are explicitly named in non-intervention doctrine as outside the scope of legitimate international concern, making their victimization doubly enclosed—by the state and by the legal framework supposedly protecting populations.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, persecuted_minorities, payer,
    powerless, biographical, identity_locked, national).

% Would challenge the doctrine on grounds that populations' right to be protected from atrocity overrides territorial inviolability. They are excluded from the framework that defends the doctrine, cannot invoke it, and face the counter-argument that their intervention itself violates non-interference. Their exclusion is maintained by the doctrine's definitional closure: if protection overrides non-intervention, the doctrine ceases to be absolute.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_humanitarian_advocates, excluded,
    organized, generational, constrained, global).

% Adjudicate claims to sovereignty and intervention legitimacy (International Court of Justice, UN Security Council, regional courts). They occupy the analytical seat, observing the structural conflict between the non-intervention doctrine and human rights frameworks. Their power is significant but constrained by the fact that great powers can veto enforcement (Security Council) or ignore rulings (declining Court jurisdiction), so their analytical authority does not translate to enforcement capacity over the beneficiaries.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_legal_authorities, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, legible rule prohibiting external territorial interference: states can conduct their internal affairs without fear of foreign invasion or regime change on humanitarian pretext. The rule was built to prevent great-power abuse of humanitarian intervention as cover for territorial conquest and to establish formal equality among states regardless of size or regime type.
% TRANSFER_FUNCTION: Transfers the right to conduct internal repression, resource extraction, and population control from populations (who would otherwise have recourse to external protectors) to state elites. Simultaneously transfers the risk of intervention from great powers (who now enjoy reciprocal non-interference) to authoritarian regimes and their victims. The extraction is measured in lost recourse to external protection, lost standing in international legal proceedings, and lost ability to appeal intervention when facing atrocity.
% ABSENT_VOICES: Persecuted populations under atrocity are structurally excluded—they are internal to the system the doctrine governs, yet denied standing within it. International humanitarian organizations are excluded by definition (the doctrine defines them as interfering). Secessionist movements and indigenous peoples claiming self-determination are excluded because the doctrine treats the existing state's borders as legally inviolable. Competing regional powers who might intervene to protect co-ethnics or allies are excluded by the doctrine's reciprocal constraint on all actors.
% DISAPPEARANCE_RATIONALE: If absolute non-intervention disappeared, the international legal order would fragment into competing interventions—great powers would cite humanitarian grounds to support proxy forces, minorities would appeal for external protection, and territorial borders would become contested as subject to remedial secession and intervention. The stability the doctrine provides (predictable non-interference, territorial inviolability, formal equality of sovereigns) would collapse, forcing emergence of new rules calibrating when intervention is legitimate. The beneficiaries' extraction (protected space for domestic repression, insulated great-power status) would evaporate immediately.
% FOUNDING_PROBLEM: After WWII and decolonization, the international system faced the risk that large powers would invoke human rights, historical claims, or civilizing missions as pretexts for territorial conquest and regime change. The doctrine was formalized to prevent the strong from dominating the weak through intervention rhetoric, establishing that each state controls its internal affairs regardless of regime type or internal conduct.
% FOUNDING_PROBLEM_CORROBORATION: The foundational problem—preventing great-power conquest under humanitarian cover—has been substantially mitigated by nuclear deterrence, economic interdependence, and mutual assurance mechanisms. Contemporary testimony from international law scholars and humanitarian organizations (external to the doctrine's beneficiaries) consistently observes that the founding threat (unbridled intervention by conquerors) has been replaced by a different problem: the doctrine now protects regimes committing mass atrocity precisely because intervention is declared illegitimate. Great powers routinely break the doctrine when strategically convenient (Syria intervention proposals, Libya, humanitarian exception rhetoric) while invoking it when it serves them, indicating the founding justification has decoupled from current function.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as snare (pure extraction) rather than tangled rope because its persistence depends entirely on coercion and exclusion of alternatives, not on genuine coordination benefits at the victim end. The extractiveness metric (0.82, rising from 0.62 over 75 years) reflects that the doctrine's function has shifted from preventing conquest to enabling domestic repression. The theater ratio (0.48, rising from 0.28) indicates nearly half of enforcement activity is performative: invoking the doctrine as legitimacy cover while great powers routinely violate it when strategically convenient. Suppression (0.71) is structural—maintained through legal definition (declaring victims internal), institutional practice (Security Council vetoes on intervention), and state monopoly on force (victims cannot appeal). The accessibility-collapse divergence is stark: at the structural level (great powers, legal system), alternatives remain (humanitarian intervention, sovereignty-conditional frameworks); at the individual level (persecuted persons), exit is eliminated—they cannot change their identity, geography, or legal status as internal subjects. Rising extractiveness over the interval reflects accumulation: as Cold War constraints eased and humanitarian intervention became increasingly discussed, the doctrine hardened its defense, intensifying the extraction against those seeking protection. Theater ratio's plateau at t=75 suggests the doctrine's legitimacy maintenance has stabilized at near-maximum performativity—it is invoked constantly but violated routinely, which is the signature of a constraint sustained by inertia and beneficiary coordination rather than consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the authoritarian-elite seat (agenda-setter), the doctrine is essential governance infrastructure—it prevents external interference in legitimate state administration and preserves sovereignty as the foundational principle of international order. From the great-power seat (beneficiary with arbitrage exit), the doctrine is selectively invoked—used to protect against ICC prosecution or interference when convenient, abandoned in favor of humanitarian exception rhetoric when strategic interests align with intervention. From the persecuted-population seat (powerless payer), the doctrine is imprisonment—it defines their suffering as outside the scope of legitimate international concern and forecloses the one avenue (external protection) they might access. The engine computes these as seat-specific types from the structural data (power atom, exit options, beneficiary/victim role): an institutional beneficiary with arbitrage exit will compute as receiving effective subsidy from the constraint, while a powerless payer with trapped exit will compute as bearing full extraction. The perspectival gap is the engine's discovery of this asymmetry, not an authored classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from power, exit options, and beneficiary/victim status. Authoritarian elites: institutional power + trapped exit (they cannot exit state sovereignty; it is constitutive of their authority) + agenda-setter role + declared beneficiary → d near 0.0 (full beneficiary, effective subsidy). Great powers: institutional power + arbitrage exit (can invoke or violate the doctrine as strategy demands) + declared beneficiary → d approximately 0.15 (net beneficiary, some vulnerability to constraint). Persecuted populations: powerless + trapped exit (cannot change identity, geography, or internal status) + declared victim → d near 1.0 (full target, pure extraction). Humanitarian advocates: organized power + constrained exit (cannot invoke the doctrine, face counter-argument that their intervention violates it) + excluded role → d approximately 0.85 (nearly full target). The asymmetry is structural: the constraint's persistence depends on maintaining high d for victims (ensuring they have no effective alternatives) and low d for beneficiaries (ensuring they can switch between invoking and violating it without cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power conquest under humanitarian cover) has moved from live to dead status. The doctrine persists not because it solves the founding problem—nuclear deterrence, economic interdependence, and mutual assured destruction are the primary constraints on conquest—but because it solves a different, narrower problem: it allows state elites to claim territorial immunity from accountability and allows great powers to maintain formal equality while preserving strategic intervention rights. The constraint exhibits classic mandatrophy: its original function (prevent conquest) has atrophied, replaced by a different function (enable domestic repression, provide legal fig leaf for selective intervention). Theater ratio climbing from 0.28 to 0.48 is the diagnostic marker: enforcement energy is increasingly devoted to maintaining the doctrine's legitimacy rather than to actual non-interference, which is routinely violated. The doctrine is no longer justified on its founding grounds but has become self-justifying (because it exists, sovereignty is categorical; because sovereignty is categorical, the doctrine must be upheld). An honest mandatrophy resolution would acknowledge the founding problem dead and replace the constraint with a conditional sovereignty framework (the sibling reading) or graded sovereignty (the other sibling), but the agenda-setter beneficiaries have no incentive to trigger that replacement, so the doctrine persists as theatrical performance maintained by institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_substitution,
    'Has the constraint''s actual function shifted from preventing great-power conquest (founding problem) to enabling state elite insulation from accountability (contemporary function)?',
    'Historical analysis of intervention justifications pre- and post-Cold War; documentation of violation patterns by beneficiaries (great powers invoke the doctrine when it suits them, violate it when it does not); comparison of actual non-interference rates against predicted rates under competing doctrines (e.g., conditional responsibility).',
    'If true, the constraint has moved from coordination (solving a real collective-action problem around conquest risk) to pure extraction (protecting elites while victimizing populations). This would support reclassification from tangled rope (if it ever coordinated at the beneficiary end) to snare, and would identify the constraint as a mandatrophy candidate (function dead, doctrine persisting via inertia).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_substitution, empirical, 'Whether the constraint''s founding problem persists or has been supplanted by a different extractive function.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of persecuted populations maintained by structural barriers (legal definitions, state monopoly on force, international institutional rules) or by internalized acceptance of the doctrine as legitimate?',
    'Post-exit trajectory analysis: if populations who escape the territory (refugees, exiles) cease to accept the doctrine''s legitimacy and support intervention on their behalf, suppression is primarily structural. If they continue to accept the doctrine as binding even after escape, suppression is partly internalized (identity-fusion with the legal system itself).',
    'If suppression is primarily structural, the constraint''s effective extraction can be reduced by changing institutional rules (codifying intervention rights, expanding ICC jurisdiction, establishing humanitarian exception). If suppression is internalized, beneficiary coordination around the doctrine''s legitimacy is stronger and structural change alone would not eliminate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the doctrine''s suppressive force rests on external institutional barriers or on internalized acceptance by its victims.').

omega_variable(
    great_power_arbitrage_instability,
    'Can great powers genuinely maintain arbitrage exit (invoking non-interference when convenient, violating it when convenient) indefinitely, or does consistent selective violation eventually collapse the doctrine''s legitimacy?',
    'Monitoring violation-to-invocation ratios over time; documentation of institutional responses (does the constraint harden enforcement or tolerate violation?); observation of whether smaller powers develop counter-doctrines asserting intervention rights against great powers.',
    'If arbitrage is stable, the constraint persists indefinitely with great powers as net beneficiaries. If arbitrage is unstable, either the doctrine will harden (moving closer to pure enforcement against weaker states) or collapse (replaced by a conditional or graded framework). Either path would increase manifested asymmetry, strengthening snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(great_power_arbitrage_instability, empirical, 'Whether the constraint''s asymmetry between great-power arbitrage and population exclusion is sustainable long-term.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Is the distinction between this reading (absolute_non_intervention) and the conditional_responsibility reading fundamentally about empirical beliefs (whether internal atrocity triggers intervention legitimacy) or normative commitments (whether sovereignty categorical or conditional)?',
    'Testing whether the readings could coexist in a single framework where conditional intervention applied only to defined atrocity thresholds (e.g., genocide triggers exception, ordinary repression does not): if the readings diverge over the threshold rather than over whether intervention can ever be legitimate, the distinction is empirically grounded (where is the boundary?). If the readings diverge over whether any exception to non-interference is permissible (absolute vs. never-absolute), the distinction is normative (foundational axiom).',
    'If the distinction is empirical-boundary-setting, the readings might coexist more easily through negotiated thresholds (many real legal frameworks use this approach: ICC jurisdiction, humanitarian law triggering rules). If normative, the readings foreclose each other (absolute cannot accommodate conditional) and the kernel cannot be resolved through threshold calibration alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether the sibling readings disagree on empirical thresholds or on foundational normative principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(west_tr_t0, observed).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(west_tr_t10, observed).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(west_tr_t20, observed).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(west_tr_t30, observed).
narrative_ontology:measurement(west_tr_t45, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 45, 0.47).
narrative_ontology:measurement_basis(west_tr_t45, observed).
narrative_ontology:measurement(west_tr_t60, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 60, 0.48).
narrative_ontology:measurement_basis(west_tr_t60, observed).
narrative_ontology:measurement(west_tr_t75, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 75, 0.48).
narrative_ontology:measurement_basis(west_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(west_be_t0, observed).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(west_be_t10, observed).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(west_be_t20, observed).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(west_be_t30, observed).
narrative_ontology:measurement(west_be_t45, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 45, 0.81).
narrative_ontology:measurement_basis(west_be_t45, observed).
narrative_ontology:measurement(west_be_t60, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 60, 0.82).
narrative_ontology:measurement_basis(west_be_t60, observed).
narrative_ontology:measurement(west_be_t75, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 75, 0.82).
narrative_ontology:measurement_basis(west_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(west_su_t0, observed).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(west_su_t10, observed).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(west_su_t20, observed).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(west_su_t30, observed).
narrative_ontology:measurement(west_su_t45, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 45, 0.7).
narrative_ontology:measurement_basis(west_su_t45, observed).
narrative_ontology:measurement(west_su_t60, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(west_su_t60, observed).
narrative_ontology:measurement(west_su_t75, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 75, 0.71).
narrative_ontology:measurement_basis(west_su_t75, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=75
narrative_ontology:measurement(west_grid_01, westphalia_sovereignty__absolute_non_intervention, accessibility_collapse(class), 0, 0.82).
narrative_ontology:measurement(west_grid_02, westphalia_sovereignty__absolute_non_intervention, accessibility_collapse(class), 75, 0.88).
narrative_ontology:measurement(west_grid_03, westphalia_sovereignty__absolute_non_intervention, accessibility_collapse(individual), 0, 0.88).
narrative_ontology:measurement(west_grid_04, westphalia_sovereignty__absolute_non_intervention, accessibility_collapse(individual), 75, 0.92).
narrative_ontology:measurement(west_grid_05, westphalia_sovereignty__absolute_non_intervention, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(west_grid_06, westphalia_sovereignty__absolute_non_intervention, accessibility_collapse(organizational), 75, 0.74).
narrative_ontology:measurement(west_grid_07, westphalia_sovereignty__absolute_non_intervention, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(west_grid_08, westphalia_sovereignty__absolute_non_intervention, accessibility_collapse(structural), 75, 0.81).
narrative_ontology:measurement(west_grid_09, westphalia_sovereignty__absolute_non_intervention, resistance(class), 0, 0.68).
narrative_ontology:measurement(west_grid_10, westphalia_sovereignty__absolute_non_intervention, resistance(class), 75, 0.62).
narrative_ontology:measurement(west_grid_11, westphalia_sovereignty__absolute_non_intervention, resistance(individual), 0, 0.52).
narrative_ontology:measurement(west_grid_12, westphalia_sovereignty__absolute_non_intervention, resistance(individual), 75, 0.44).
narrative_ontology:measurement(west_grid_13, westphalia_sovereignty__absolute_non_intervention, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(west_grid_14, westphalia_sovereignty__absolute_non_intervention, resistance(organizational), 75, 0.52).
narrative_ontology:measurement(west_grid_15, westphalia_sovereignty__absolute_non_intervention, resistance(structural), 0, 0.45).
narrative_ontology:measurement(west_grid_16, westphalia_sovereignty__absolute_non_intervention, resistance(structural), 75, 0.38).
narrative_ontology:measurement(west_grid_17, westphalia_sovereignty__absolute_non_intervention, stakes_inflation(class), 0, 0.78).
narrative_ontology:measurement(west_grid_18, westphalia_sovereignty__absolute_non_intervention, stakes_inflation(class), 75, 0.84).
narrative_ontology:measurement(west_grid_19, westphalia_sovereignty__absolute_non_intervention, stakes_inflation(individual), 0, 0.85).
narrative_ontology:measurement(west_grid_20, westphalia_sovereignty__absolute_non_intervention, stakes_inflation(individual), 75, 0.91).
narrative_ontology:measurement(west_grid_21, westphalia_sovereignty__absolute_non_intervention, stakes_inflation(organizational), 0, 0.42).
narrative_ontology:measurement(west_grid_22, westphalia_sovereignty__absolute_non_intervention, stakes_inflation(organizational), 75, 0.48).
narrative_ontology:measurement(west_grid_23, westphalia_sovereignty__absolute_non_intervention, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(west_grid_24, westphalia_sovereignty__absolute_non_intervention, stakes_inflation(structural), 75, 0.62).
narrative_ontology:measurement(west_grid_25, westphalia_sovereignty__absolute_non_intervention, suppression(class), 0, 0.62).
narrative_ontology:measurement(west_grid_26, westphalia_sovereignty__absolute_non_intervention, suppression(class), 75, 0.72).
narrative_ontology:measurement(west_grid_27, westphalia_sovereignty__absolute_non_intervention, suppression(individual), 0, 0.68).
narrative_ontology:measurement(west_grid_28, westphalia_sovereignty__absolute_non_intervention, suppression(individual), 75, 0.78).
narrative_ontology:measurement(west_grid_29, westphalia_sovereignty__absolute_non_intervention, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(west_grid_30, westphalia_sovereignty__absolute_non_intervention, suppression(organizational), 75, 0.61).
narrative_ontology:measurement(west_grid_31, westphalia_sovereignty__absolute_non_intervention, suppression(structural), 0, 0.48).
narrative_ontology:measurement(west_grid_32, westphalia_sovereignty__absolute_non_intervention, suppression(structural), 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__absolute_non_intervention, 0.18).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, humanitarian_intervention_legitimacy).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, state_accountability_immunity).

% DUAL FORMULATION NOTE:
% The westphalia_sovereignty kernel decomposes into three constraint stories instantiating distinct readings: absolute_non_intervention (this story), conditional_responsibility (sovereignty conditional on atrocity prevention), and graded_sovereignty (sovereignty as scalar calibrating to state capacity). Each reading instantiates a different ε and beneficiary/victim structure. This story (absolute_non_intervention) treats internal atrocity as an internal matter and derives high extraction for persecuted populations. The conditional_responsibility reading would treat atrocity as sovereignty-violating and derive lower extraction for that same population. The constraint family is linked via affects_constraints to enable contamination analysis: if one reading's legitimacy erodes, the others' scope shifts. Sibling constraint stories MUST be authored separately (per ε-invariance); this file carries the absolute_non_intervention reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
