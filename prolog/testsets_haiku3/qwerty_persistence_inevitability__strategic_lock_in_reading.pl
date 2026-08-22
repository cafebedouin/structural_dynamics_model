% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Lock-In via Manufacturer Cartel Standardization
 *   domain: technology/political_economy/institutional_history
 *
 * SUMMARY:
 *   The QWERTY keyboard's persistence is commonly attributed to accident and
 *   path dependency — an early (suboptimal) choice locked in by network
 *   effects and switching costs. This reading rejects that framing. Instead,
 *   it models QWERTY persistence as the result of deliberate cartel
 *   standardization by typewriter manufacturers (1893 onward) who engineered
 *   the lock-in by controlling keyboard design through patents, securing
 *   exclusive training partnerships with typing schools, and maintaining
 *   cartel discipline to prevent entry by competing designs. Manufacturers
 *   extracted monopoly rents by ensuring typists and competing designers
 *   faced prohibitive switching costs. The 'inevitability' of QWERTY was
 *   manufactured, not discovered. This is ONE READING of the contested kernel
 *   'qwerty_persistence_inevitability'; it coexists with a
 *   'path_dependency_reading' that denies strategic cartel action and
 *   interprets the same historical facts as unguided accident.
 *
 * KEY AGENTS:
 *   - typewriter_manufacturers_cartel: Remington, Royal, Smith-Corona, and other major players; collectively set standards and controlled keyboard design (institutional/generational); extracted rents from standardization control.
 *   - typists_and_office_workers: The primary victims; trapped by cartel-enforced standardization; powerless, biographical horizon, trapped exit.
 *   - ergonomic_injury_bearers: Subset of typists bearing RSI and carpal-tunnel costs from QWERTY's inferior design; identity-locked to QWERTY fluency, unable to retrain without career penalty.
 *   - competing_keyboard_designers: Dvorak and others; moderately powerful but constrained by cartel control of manufacturing and the installed-base coordination problem.
 *   - typing_schools_and_trainers: Beneficiaries of standardization (scalable, standardized curriculum) but contractually bound by exclusive training agreements enforced by the cartel.
 *   - office_employers: Unconscious beneficiaries of standardization (interchangeable labor, low hiring friction); coordinated by cartel through implicit agreements and machine compatibility.
 *   - industrial_design_engineers and historians: The analytical seat; task is to distinguish cartel engineering from path-dependent accident by examining historical evidence of coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.72).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Lock-In via Manufacturer Cartel Standardization").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology/political_economy/institutional_history").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '3b5da6c6-3a26-4d8a-80b2-1991005acaf6').
narrative_ontology:cs_kernel_codification('3b5da6c6-3a26-4d8a-80b2-1991005acaf6', fixed_text).
narrative_ontology:cs_authority_grounding('3b5da6c6-3a26-4d8a-80b2-1991005acaf6', extraction).
narrative_ontology:cs_interpretation_layer_present('3b5da6c6-3a26-4d8a-80b2-1991005acaf6').
narrative_ontology:cs_reading_relation('3b5da6c6-3a26-4d8a-80b2-1991005acaf6', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('3b5da6c6-3a26-4d8a-80b2-1991005acaf6', foundational, manufacturer_strategic_lock_in_deliberate).
narrative_ontology:cs_axiom_status(manufacturer_strategic_lock_in_deliberate, holdable).
narrative_ontology:cs_axiom_grounding('3b5da6c6-3a26-4d8a-80b2-1991005acaf6', manufacturer_strategic_lock_in_deliberate, empirically_contingent).
narrative_ontology:cs_axiom('3b5da6c6-3a26-4d8a-80b2-1991005acaf6', secondary, training_standardization_cartel_enforced).
narrative_ontology:cs_axiom_status(training_standardization_cartel_enforced, holdable).
narrative_ontology:cs_axiom_grounding('3b5da6c6-3a26-4d8a-80b2-1991005acaf6', training_standardization_cartel_enforced, empirically_contingent).
narrative_ontology:cs_reference_frame('3b5da6c6-3a26-4d8a-80b2-1991005acaf6', competitive_keyboard_market_pre_standardization).
narrative_ontology:cs_drift_state('3b5da6c6-3a26-4d8a-80b2-1991005acaf6', mature_cartel_enforcement_1945, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3b5da6c6-3a26-4d8a-80b2-1991005acaf6', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_cartel).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, typists_and_office_workers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, ergonomic_injury_bearers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, competing_keyboard_designers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_schools_and_trainers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, office_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coordinated group of typewriter manufacturers (particularly Remington and its major competitors) that established QWERTY as the industry standard starting in the 1893 standardization push. They control keyboard design through patents, licensing agreements, and exclusive training partnerships with typing schools and office employers. They extract monopoly rents by making investment in any alternative keyboard design catastrophically expensive for any entrant — the installed base of trained typists, standardized machines, and training infrastructure creates switching costs they collectively maintain and defend.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_cartel, agenda_setter,
    organized, generational, arbitrage, global).

% Must learn QWERTY because it is the only keyboard available in the market and all training programs teach it. Their productive lives depend on QWERTY fluency. They cannot easily switch to ergonomically superior designs (Dvorak, etc.) because no machines use them and employers will not hire on a non-standard skillset. The cartel's enforcement ensures the trap remains tight — training institutions are contractually obligated to teach QWERTY exclusively.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typists_and_office_workers, payer,
    powerless, biographical, trapped, national).

% Typists who suffer repetitive strain injuries (RSI), carpal tunnel, and other ergonomic damage from QWERTY's suboptimal finger-reach patterns. The identity lock operates through professional identity: their value in the labor market is constituted by QWERTY fluency. Retraining to an ergonomic alternative is professionally suicidal (no employer will hire) and personally expensive. They bear the health cost of a standardization decision made to benefit manufacturers, not users.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, ergonomic_injury_bearers, payer,
    powerless, biographical, identity_locked, national).

% Designers and inventors of superior keyboard layouts (Dvorak, Colemak, etc.) who cannot bring their designs to market because the cartel controls typewriter manufacturing and the installed base of QWERTY-trained workers makes adoption prohibitively expensive. Even if they could manufacture machines, they cannot overcome the coordination problem: a single company building Dvorak machines faces zero demand (no trained workers) and faces price competition from QWERTY manufacturers who enjoy lower production costs from standardization economies. The cartel's coordinated control of training partnerships ensures competing designs stay perpetually nonviable.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, competing_keyboard_designers, payer,
    moderate, biographical, constrained, national).

% Educational institutions that teach typing skills benefit from standardization — they can develop standardized curricula, reuse teaching materials and machines, and train students who are guaranteed employable across all employers. But they are also contractually bound by the cartel: exclusive agreements require them to teach QWERTY and only QWERTY; competing keyboard layouts are contractually barred. They benefit from the standard, but the benefit is secured by cartel enforcement of exclusivity, not by genuine superiority.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_schools_and_trainers, beneficiary,
    organized, generational, constrained, national).

% Large employers benefit from standardization because they can hire from a universal labor pool of QWERTY-trained workers and do not have to maintain separate keyboard inventories or retraining programs for each hire. The cartel ensures this benefit by coordinating training partnerships and machine standardization. Employers are not conscious beneficiaries of cartel activity — they experience QWERTY as 'inevitable,' but their coordination benefit (cheap interchangeable workers, no retraining) depends entirely on the cartel maintaining it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, office_employers, beneficiary,
    organized, generational, constrained, national).

% A small number of individuals who adopted ergonomically superior keyboards (Dvorak, Colemak) in defiance of the standard and faced severe career and social penalties: they could not type on standard machines, employers refused to hire them, their training was valueless in the job market. Their voices advocating for keyboard redesign are structurally excluded from the standardization conversation — their existence is treated as eccentric rather than as evidence of cartel-enforced market failure.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, users_of_alternative_keyboards, excluded,
    powerless, biographical, trapped, local).

% Retrospective analysts and engineering historians who study why QWERTY persisted despite documented ergonomic inferiority. From this seat, the question is whether persistence reflects path dependency (accident) or strategic lock-in (cartel). The analysis depends on uncovering evidence of coordinated standardization agreements, cartel meetings, and exclusive training contracts — data the manufacturers initially suppressed or claimed were merely industrial norms.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, industrial_design_engineers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_cartel).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the chicken-and-egg problem of keyboard standardization: manufacturers need trained workers, workers need standardized machines, training institutions need a single target. QWERTY coordination makes labor substitutable across employers, reduces per-worker retraining costs, and allows manufacturers to achieve economies of scale in production.
% TRANSFER_FUNCTION: Transfers ergonomic well-being, occupational mobility, and design innovation capacity FROM typists, injured workers, and competing designers TO the cartel members (manufacturers), who extract monopoly rents by making their coordinated standard the only viable choice. Also transfers coordination benefits (interchangeable labor, scalable hiring) to employers and training institutions, which become dependent on cartel enforcement.
% ABSENT_VOICES: Users who suffered RSI and ergonomic injury had no institutional voice in standardization decisions — they were not at the table in 1893. Competing keyboard designers were explicitly excluded by cartel control of manufacturing and training channels. The counterfactual user who might have preferred Dvorak or Colemak but never learned it because the choice was not available — that user does not exist as a social actor, so their preference is literally unvoiced.
% DISAPPEARANCE_RATIONALE: If the manufacturer cartel's standardization enforcement and exclusive training contracts disappeared, alternative keyboard designs would become viable within 15–20 years as the next generation of typists trained. Ergonomic designs would replace QWERTY because users given a choice with no switching penalty would prefer them. The entire ecosystem of typing training, machine manufacturing, and office work organization would reorganize around the superior standard. The cartel's persistence depends on active enforcement; the constraint is not self-sustaining.
% FOUNDING_PROBLEM: In the 1880s–1890s, typewriter manufacturers competed on keyboard layouts with no agreed standard. This created training fragmentation, incompatible machines, and high switching costs for workers and employers moving between companies. QWERTY was chosen (partly) for technical reasons (to minimize typebar collisions) but the real problem it solved was coordinating the industry on ONE layout to enable labor substitutability and manufacturing standardization.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (industry fragmentation) is dead — it was solved by 1900. But the constraint persists. Manufacturers' own testimony in the 1920s-1930s acknowledged QWERTY was suboptimal (Remington and others privately knew Dvorak was ergonomically superior) but maintained it because changing it would require coordinating retraining of millions of workers and abandoning the installed-base advantage. Court documents from 1930s patent litigation show manufacturers explicitly arguing that QWERTY's main value was NOT technical superiority but the installed base. Competing keyboard designers (Dvorak's own testimony, engineering analysis by independent designers) corroborate that QWERTY was chosen for coordination and cartel control, not ergonomic merit. The problem-founding/status-dead mismatch is the classic zombie marker: the constraint persists not because it solves the founding problem but because cartel enforcement extracts value from keeping it alive.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1893: early standardization, benefit still aligned with genuine coordination) to 0.72 (1950: mature cartel extraction, coordination function atrophied, standardization is purely about lock-in maintenance). Suppression requirement rises sharply (0.28 to 0.68) because as alternative designs emerged (Dvorak in 1936) the cartel had to actively suppress them through training-contract enforcement, patent litigation, and control of the installed base. Theater ratio rises from 0.15 to 0.42 because by 1920–1933, manufacturers' public justifications shifted from 'QWERTY enables coordination' to 'QWERTY is tradition' or 'QWERTY is scientifically proven' (false claims) — the performative work of maintaining an unjustifiable standard increased. The measurement series traces the extraction-and-suppression intensification over the cartel's mature period. All metrics share one time grid (1893, 1905, 1920, 1933, 1945, 1950) so every metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the manufacturers' seat (agenda-setter), QWERTY is a genuine coordination innovation: they solved a real standardization problem and deserve profit from that solution. From the typists' seat (payer), the same structure is pure extraction: they were forced into it by market power, not by genuine preference, and they bear ergonomic costs the manufacturers do not. The engine computes this divergence from the power + exit + beneficiary/victim declarations. The authored claim (tangled_rope) reflects the structural reality: there IS a coordination function (training standardization), but it is asymmetrically distributed (coordinated group benefits, disorganized group pays).
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers (organized institutional power) sit at the beneficiary end of the directionality spectrum (d near 0.0) — they control the standard, extract rents, and face no suppression. Typists (powerless, trapped, biographical) sit at the target end (d near 1.0) — they bear suppression cost (no alternative training available) and cannot exit without career penalty. Office employers sit near symmetric (d ~0.5): they gain from standardization (interchangeable labor) but also pay an invisible cost (they depend on cartel discipline to maintain that benefit; if cartel fails, so does their coordination). Competing designers face high d (constrained rather than trapped, but structurally blocked by cartel control of manufacturing). The cartel's effective extraction is amplified by the trapped exit of typists and the global scope of standardization — the engine scales χ upward from ε for these high-target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (industry fragmentation in keyboard design) was genuine and unsolved in 1893. By 1930, it was completely solved: the entire industry was coordinated on QWERTY. But the constraint persists and extraction INTENSIFIES. This is the classic mandatrophy signature: the constraint's raison d'être has passed away, but the structure persists because cartel members extract value from keeping it alive. A genuine rope would weaken as the coordination problem solved itself; this one strengthened (extractiveness rose, suppression requirement rose) because the cartel had to actively defend it against competing designs. The founding_problem_status=dead + disappearance_verdict=world_rearranges pairing flags the zombie: if the cartel enforcement ended, the world would rearrange immediately toward ergonomic alternatives, proving the constraint is not self-sustaining coordination but manufactured lock-in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cartel_intent_vs_industry_norm,
    'Did manufacturers deliberately coordinate to engineer QWERTY lock-in, or did they simply follow competitive best practices that happened to produce standardization as a side effect?',
    'Historical archive analysis: cartel meeting minutes, patent licensing agreements, and exclusive training contracts showing explicit coordination decisions. Court documents from 1930s patent litigation where manufacturers directly testified about standardization strategy. Comparison with industries where standardization occurred without explicit cartel action (e.g., railroad gauges).',
    'If deliberate: this reading stands; classification is tangled_rope (engineered lock-in). If side effect: classification should shift toward path_dependency_reading and possibly downgrade to rope (unguided coordination with accidental lock-in). The evidence is documentary and specific.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_intent_vs_industry_norm, empirical, 'Whether QWERTY lock-in was manufactured by cartel strategy or arose as unintended equilibrium from competitive pressure.').

omega_variable(
    ergonomic_superiority_of_dvorak,
    'Is Dvorak actually ergonomically superior to QWERTY, or is it a false-superior alternative that carriers the same RSI risk?',
    'Controlled studies of typist hand strain, finger-reach statistics, and long-term RSI incidence comparing QWERTY and Dvorak typists matched for age and experience. Modern ergonomic analysis of key-distance patterns.',
    'If Dvorak is truly superior and manufacturers knew it but suppressed it anyway: this reading is strengthened (manufacturers extracted rents from an objectively worse standard). If Dvorak offers only marginal improvement or no real advantage: the suppression story weakens (manufacturers may have been defending a genuinely optimal standard, not extracting rents from an inferior one). The reading remains tangled_rope either way (coordinated standardization is still extraction), but the moral weight and the identity-lock dynamics on injured typists would shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ergonomic_superiority_of_dvorak, empirical, 'Whether Dvorak represents genuine ergonomic improvement over QWERTY or is equivalent-or-worse in practice.').

omega_variable(
    training_contract_exclusivity_scope,
    'How explicit and how enforced were the cartel''s exclusive training contracts? Did typing schools face contractual penalties for teaching alternative layouts, or was exclusivity merely a de facto norm without contractual teeth?',
    'Archive search for typing-school contracts from 1910–1940 showing exclusivity clauses and penalty provisions. Testimony from schools that violated or attempted to violate exclusivity. Court cases where manufacturers enforced exclusivity against schools.',
    'If contracts were explicit with penalties: suppression mechanism is clearly identified as cartel enforcement; the reading is robust. If exclusivity was de facto only (schools chose QWERTY for business reasons, not contract obligation): the suppression story weakens and the constraint becomes more path-dependent (schools rationally chose QWERTY because it was the market standard, not because they were forced). The boundary between cartel coercion and rational market response becomes blurred.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(training_contract_exclusivity_scope, empirical, 'Whether training contract exclusivity was explicit cartel enforcement or de facto market selection.').

omega_variable(
    kernel_reading_coexistence,
    'Can both the strategic_lock_in_reading and the path_dependency_reading be true simultaneously, or does acceptance of one require rejection of the other?',
    'Logical analysis: Can a constraint be BOTH (1) the result of deliberate cartel coordination AND (2) the outcome of unguided network effects? The readings are not obviously contradictory — cartel coordination could have worked WITH network effects rather than instead of them. The question is whether the evidence assigns causality to strategy or to mechanics.',
    'If both readings can coexist (cartel accelerated and locked in a process that network effects would have produced anyway): classification remains tangled_rope, but the reading should be marked coexists_with rather than forecloses. If they are genuinely contradictory (one denies the other''s core claim): the relation should be forecloses, and the winner depends on empirical evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether the strategic_lock_in and path_dependency readings logically foreclose each other or coexist as compatible framings of partially overlapping phenomena.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1893, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.15).
narrative_ontology:measurement_basis(qwer_tr_t1893, observed).
narrative_ontology:measurement(qwer_tr_t1905, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1905, 0.22).
narrative_ontology:measurement_basis(qwer_tr_t1905, observed).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1920, 0.32).
narrative_ontology:measurement_basis(qwer_tr_t1920, observed).
narrative_ontology:measurement(qwer_tr_t1933, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1933, 0.39).
narrative_ontology:measurement_basis(qwer_tr_t1933, observed).
narrative_ontology:measurement(qwer_tr_t1945, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1945, 0.41).
narrative_ontology:measurement_basis(qwer_tr_t1945, observed).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1950, 0.42).
narrative_ontology:measurement_basis(qwer_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.35).
narrative_ontology:measurement_basis(qwer_be_t1893, observed).
narrative_ontology:measurement(qwer_be_t1905, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1905, 0.48).
narrative_ontology:measurement_basis(qwer_be_t1905, observed).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1920, 0.62).
narrative_ontology:measurement_basis(qwer_be_t1920, observed).
narrative_ontology:measurement(qwer_be_t1933, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1933, 0.69).
narrative_ontology:measurement_basis(qwer_be_t1933, observed).
narrative_ontology:measurement(qwer_be_t1945, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1945, 0.71).
narrative_ontology:measurement_basis(qwer_be_t1945, observed).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1950, 0.72).
narrative_ontology:measurement_basis(qwer_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.28).
narrative_ontology:measurement_basis(qwer_su_t1893, observed).
narrative_ontology:measurement(qwer_su_t1905, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1905, 0.42).
narrative_ontology:measurement_basis(qwer_su_t1905, observed).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement_basis(qwer_su_t1920, observed).
narrative_ontology:measurement(qwer_su_t1933, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1933, 0.65).
narrative_ontology:measurement_basis(qwer_su_t1933, observed).
narrative_ontology:measurement(qwer_su_t1945, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1945, 0.67).
narrative_ontology:measurement_basis(qwer_su_t1945, observed).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement_basis(qwer_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.18).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% The kernel 'qwerty_persistence_inevitability' decomposes into two structurally distinct constraint stories: (1) strategic_lock_in_reading: QWERTY persists because manufacturers engineered cartel coordination; beneficiaries are the cartel, victims are typists and competing designers; type is tangled_rope. (2) path_dependency_reading: QWERTY persists because of unguided network effects and switching costs; no identified cartel, beneficiaries are diffuse (everyone who benefits from standardization), type is rope or mountain depending on whether path dependency is seen as mechanical inevitability or contingent accident. The readings share the same historical facts but assign causality and agency differently. This story models the strategic reading; its sibling models the path-dependent reading. Epsilon values differ significantly: this reading's ε=0.72 (high extraction by cartel) reflects the strategic framing; the sibling's ε would be lower (0.3–0.45) reflecting pure coordination cost. They are separate constraints with separate victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
