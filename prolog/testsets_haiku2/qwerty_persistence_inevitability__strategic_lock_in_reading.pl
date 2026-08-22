% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: QWERTY Keyboard Lock-In via Manufacturer Cartel (Strategic Lock-In Reading)
 *   domain: technology_history/political_economy/standards
 *
 * SUMMARY:
 *   This constraint story instantiates the STRATEGIC LOCK-IN READING of the
 *   QWERTY persistence kernel. Under this reading, QWERTY keyboard layout
 *   persisted not because of path-dependent accident or inherent superiority,
 *   but because manufacturers (Remington, Royal, Underwood, and others)
 *   coordinated standardization through trade associations, training-school
 *   partnerships, and patent pools, extracting rents by blocking superior
 *   alternatives from reaching critical mass adoption. The reading emphasizes
 *   engineered lock-in via control of the training pathway—not technical
 *   inevitability. The claim/metric gap is deliberate and structurally
 *   informative: the constraint is CLAIMED as tangled_rope (coordination +
 *   asymmetric extraction) while the authored metrics show rising
 *   extractiveness over time as the founding coordination problem was solved
 *   and the cartel transitioned from genuinely solving a market failure to
 *   rent extraction pure. The measurement series track this transition:
 *   base_extractiveness rises from 0.15 in 1890 (when coordination justified
 *   some arrangement) to 0.68 by 1960 (when coordination is fully
 *   accomplished and extraction dominates). Theater_ratio rises from nearly
 *   zero (early enforcement serves real coordination) to 0.42 (growing
 *   maintenance activity defending market control rather than solving the
 *   founding problem). Suppression_requirement parallels: enforcement needed
 *   to maintain coordination agreement in early period (1890-1905), but by
 *   1920-1960 enforcement is needed to prevent defection to superior
 *   alternatives—the constraint's function has shifted from solving to
 *   suppressing.
 *
 * KEY AGENTS:
 *   - keyboard_manufacturers_1893_cartel: Remington, Royal, Underwood and others; coordinated standardization via trade associations and training partnerships
 *   - professional_typists: Learned QWERTY as identity-locked labor market entry; bore ergonomic costs and retraining barriers
 *   - clerical_workers: Powerless entry gate (typing schools); paid physical costs of a locked-in suboptimal layout
 *   - typing_schools_and_educators: Benefited from manufacturer funding; became unwitting lock-in enforcers by gatekeeping training
 *   - alternative_layout_inventors: Dvorak (1932) and others; systematically excluded from institutional pathways despite superior designs
 *   - labor_economists_and_historians: Observer seats; adjudicate between strategic vs. path-dependent explanations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.71).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Keyboard Lock-In via Manufacturer Cartel (Strategic Lock-In Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/standards").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, 'c42e527a-618f-4357-81ec-da5ee3977e9b').
narrative_ontology:cs_kernel_codification('c42e527a-618f-4357-81ec-da5ee3977e9b', implicit).
narrative_ontology:cs_authority_grounding('c42e527a-618f-4357-81ec-da5ee3977e9b', extraction).
narrative_ontology:cs_reading_relation('c42e527a-618f-4357-81ec-da5ee3977e9b', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('c42e527a-618f-4357-81ec-da5ee3977e9b', foundational, qwerty_persistence_manufactured_via_cartel_control).
narrative_ontology:cs_axiom_status(qwerty_persistence_manufactured_via_cartel_control, holdable).
narrative_ontology:cs_axiom_grounding('c42e527a-618f-4357-81ec-da5ee3977e9b', qwerty_persistence_manufactured_via_cartel_control, empirically_contingent).
narrative_ontology:cs_axiom('c42e527a-618f-4357-81ec-da5ee3977e9b', foundational, alternatives_structurally_excluded_not_emergently_selected).
narrative_ontology:cs_axiom_status(alternatives_structurally_excluded_not_emergently_selected, holdable).
narrative_ontology:cs_axiom_grounding('c42e527a-618f-4357-81ec-da5ee3977e9b', alternatives_structurally_excluded_not_emergently_selected, empirically_contingent).
narrative_ontology:cs_reference_frame('c42e527a-618f-4357-81ec-da5ee3977e9b', manufacturers_extract_rents_through_standardization_cartel).
narrative_ontology:cs_drift_state('c42e527a-618f-4357-81ec-da5ee3977e9b', contemporary, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('c42e527a-618f-4357-81ec-da5ee3977e9b', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_manufacturers_1893_cartel).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, clerical_workers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, device_adopters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_schools_and_educators).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, capital_goods_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, consumers_of_typed_output).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_schools_and_educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% In the 1890s-1910s, keyboard manufacturers (Remington, Royal, Underwood, and others) coordinated standardization of QWERTY layout through trade associations, journalist partnerships, and secretarial school training agreements. They framed QWERTY as inevitable and universal, not as a competitive choice, thereby locking in production to a single design and extracting rents through patent pools and exclusionary manufacturing standards. The cartel's power lay in controlling what typists learned—not through technical superiority but through institutional gatekeeping of training pathways.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_manufacturers_1893_cartel, agenda_setter,
    organized, generational, arbitrage, national).

% Learned QWERTY as part of professional identity formation and labor market entry. Retraining to a different layout (e.g., Dvorak, which emerged in 1932 and offered documented ergonomic improvement) was individually rational only if others also switched—a coordination problem the cartel maintained deliberately. Professional typists bore ergonomic costs (repetitive strain injury, reduced efficiency) while remaining identity-locked to QWERTY through certification, employment histories, and decades of muscle memory. The layout they learned was presented as natural and inevitable, not as a strategic choice defending manufacturer interests.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists, payer,
    moderate, biographical, identity_locked, national).

% Women entering the labor market as stenographers and clerical workers had no choice in keyboard layout; QWERTY was the gate to employment. They paid the physical cost (hand fatigue, cumulative strain) and the opportunity cost (no ability to capture efficiency gains from superior layouts) for decades. Their powerlessness meant the cartel could enforce standardization through the single institutional point where new workers entered—the typing school curriculum. No worker could unilaterally switch; coordination was suppressed.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, clerical_workers, payer,
    powerless, biographical, trapped, national).

% Commercial typing schools benefited from manufacturer sponsorship and curriculum guidance—they received equipment discounts, teaching materials, and institutional legitimacy in exchange for teaching QWERTY exclusively. Teachers and school administrators saw their institutional survival depend on aligning with manufacturer standards, which were presented as objective and universal. They became unwitting enforcers of the lock-in, unable to deviate because doing so would mean losing funding and being labeled as providing substandard training.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_schools_and_educators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_schools_and_educators, payer).

% Inventors of superior layouts (August Dvorak in 1932, and others) produced documented evidence of ergonomic and speed improvement over QWERTY. They were systematically excluded from training-school curricula, press coverage controlled by manufacturer advertising, and institutional standardization bodies where decisions were made. Their exclusion was structural—the cartel's control of the training pathway meant superior alternatives could not reach critical mass adoption, no matter their technical merit.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors, excluded,
    moderate, biographical, constrained, national).

% Industries using typewriters (banks, insurance, government offices, newspapers) benefited from standardization reducing coordination costs—all hires could type on any machine, workers were interchangeable across firms. This coordination benefit was real. However, the cartel extracted additional rent by preventing layout alternatives that would have increased productivity, meaning users captured only part of the potential gain from standardization while manufacturers captured the scarcity premium.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, capital_goods_manufacturers, beneficiary,
    organized, generational, mobile, national).

% End consumers benefited from reliable, readable typed documents produced by standardized trained typists. This coordination benefit persisted even under the cartel arrangement. However, they also bore an indirect cost: documents produced more slowly and with higher error rates than would have occurred under a superior layout, and any layout transition would have required education investment. The extraction rode on top of a real coordination function.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, consumers_of_typed_output, beneficiary,
    powerless, immediate, mobile, national).

% Analyze whether QWERTY persistence is best explained by path-dependent accident (the sibling reading) or by strategic manufacturer action (this reading). Their role is to examine contracts, trade association records, typing school partnerships, marketing campaigns, and patent coordination to determine whether the lock-in was engineered or emergent. This constraint story represents their strategic-lock-in hypothesis.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, labor_economists_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_manufacturers_1893_cartel).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized keyboard layout enables labor market mobility—a typist trained on one machine can work on any standardized machine; employers can hire trained labor flexibly; the industry avoids a fragmented market where workers and machines must be matched. This coordination problem is real and significant.
% TRANSFER_FUNCTION: Transfers ergonomic efficiency losses and retraining barriers from manufacturers to typists; transfers monopoly rents from users of typewriters to the cartel members through patent pools and design exclusivity; transfers institutional control over training from educators to manufacturers through curriculum gatekeeping.
% ABSENT_VOICES: Alternative layout designers (Dvorak and others) whose superior designs were systematically excluded from institutional training pathways and press coverage; workers who would have benefited from layout switches but were locked in by employer standardization and identity fusion; international typists whose languages might have had different optimal layouts but were forced into a single English-optimized standard.
% DISAPPEARANCE_RATIONALE: If the cartel's enforcement machinery and training-pathway control disappeared overnight, alternative layouts with documented ergonomic advantages would compete directly; within a generation, a superior layout would reach critical mass adoption; typing speed and strain injury rates would shift; the installed base of QWERTY-trained workers would gradually retire; manufacturers would diversify to competing standards or open design. The constraint exists to prevent this rearrangement.
% FOUNDING_PROBLEM: In the 1880s-1890s, typewriter manufacturers competed on design innovation; keyboards varied widely, making labor training difficult and workers uninterchangeable across firms. The founding problem was genuine: standardization solved a real coordination failure in the emerging labor market for machine operators.
% FOUNDING_PROBLEM_CORROBORATION: Manufacturers and typing schools in their own records (trade association minutes, curriculum materials) acknowledged the early coordination problem as the justification for standardization. However, economic historians and labor scholars (not manufacturer-aligned parties) have documented that by 1910-1920, the coordination problem was solved—the market had stabilized, workers were mobile, firms could interoperate—yet the cartel maintained active enforcement mechanisms (patent pools, exclusive training agreements, advertising control) precisely to prevent the transition to superior alternatives. The founding problem is dead; the constraint persists because of extraction, not coordination.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises sharply from 0.15 (1890, early coordination) to 0.54 (1920, lock-in established) to 0.68 (1960, rent extraction mature). This trajectory reflects the reading's core claim: the cartel solved a real coordination problem early (1890-1905) then deliberately maintained enforcement to prevent transitions to superior alternatives even after coordination was established. Suppression is high (0.71) because the constraint's persistence depends on active enforcement mechanisms—patent pools, exclusive training agreements, press control, secretarial-school curriculum lockdown—not on voluntary compliance or technical inevitability. Theater_ratio grows from 0.05 to 0.42 as the constraint matures: early enforcement justified as 'standardization for coordination,' by 1960 the same enforcement apparatus increasingly serves to block alternatives (Dvorak) that would improve efficiency. The gap between suppression (0.71) and theater (0.42) indicates substantial real enforcement activity, not purely performative maintenance—the cartel actively prevented defection, not merely maintained institutional inertia. Accessibility_collapse is moderate (0.64): alternatives exist and are technically superior, but the cartel's control of training pathways made them inaccessible to new entrants. Resistance is moderate (0.52): professional typists and labor economists mounted resistance (Dvorak research, efficiency studies), but the cartel's control of standards bodies and manufacturing meant resistance did not translate to alternatives reaching critical mass. The measurement grid shares one time axis (all metrics authored at all examined years) so the temporal pattern is coherent: the constraint transitions visibly from coordination-function to extraction-function over the 70-year interval.
 *
 * PERSPECTIVAL GAP:
 *   The cartel seats (manufacturers, typing schools) experience this as inevitable standardization—a coordination solution so obviously beneficial that alternatives seem irrational. From these seats, the constraint computes as rope or low-extraction tangled_rope. The victim seats (typists, workers, alternative designers) experience it as manufactured lock-in—a barrier to adoption of superior tools, justified post-hoc as natural necessity. From these seats, the constraint computes as snare or high-extraction tangled_rope. The engine computes this divergence from structural data: the cartel holds power, arbitrage exit (can shift to other markets), and holds the agenda-setting role; typists hold moderate/powerless power, identity-locked/trapped exit, and payer roles. The extraction coefficient χ should be substantially higher for typist seats than for manufacturer seats, reflecting their structural disadvantage.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard manufacturers (organized power, arbitrage exit, agenda_setter) derive d near 0.1-0.2 (strong beneficiaries). Professional typists (moderate power, identity_locked exit, payer) derive d near 0.75-0.85 (targets). Clerical workers (powerless, trapped, payer) derive d near 0.9+ (full targets). Typing schools occupy an ambiguous middle (moderate power, constrained exit, dual role beneficiary/payer)—they benefit from manufacturer partnership but are also pressured to enforce the standard, deriving d near 0.45-0.55. No directionality override is needed; the structural data (power + exit + role) drive the derivation without correction. The cartel's ability to control training pathways means they extract not only from direct transactions (typewriter sales) but also from the institutional apparatus (typing schools) that gates labor-market entry, multiplying effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint classification as tangled_rope (not snare) is justified because a genuine coordination function exists and persists throughout the interval: having a standard keyboard layout IS more efficient than fragmentation, and the constraint solves that problem. However, the measurement trajectory shows the constraint transitioning from primarily coordination-serving (early) to primarily extraction-defending (late). A pure snare reading would deny any coordination value; the strategic lock-in reading affirms coordination while emphasizing that once coordination was achieved, the cartel deliberately maintained suppression to prevent improvement—the defining tangled_rope signature. The founding_problem_status = 'dead' with disappearance_verdict = 'world_rearranges' confirms mandatrophy: the problem this constraint was built to solve is gone, but the constraint persists via enforcement, not utility. This is exactly when a constraint reclassifies from rope to piton or from tangled_rope into snare-dominated extraction. The measurement series provide evidence for the mandatrophy claim: suppression_requirement remains high even as base_extractiveness plateaus, suggesting the constraint is sustained by force, not by solved-problem equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cartel_intentionality_and_coordination,
    'Were keyboard manufacturers'' standardization efforts primarily aimed at solving the genuine coordination problem of labor market mobility, or were they primarily aimed at rent extraction via design lock-in?',
    'Archival evidence: trade-association meeting minutes, private correspondence between manufacturers, patent-pool agreements, and deliberate decisions to exclude superior alternatives. Economic analysis: did the cartel maintain enforcement mechanisms AFTER the coordination problem was solved? Did they explicitly suppress Dvorak and other alternatives despite documented superiority?',
    'If evidence shows deliberate suppression of alternatives despite solved coordination problem, the strategic lock-in reading is corroborated and the constraint classifies as tangled_rope (coordination function + extraction). If evidence shows coordination was the primary motive and suppression was incidental, the path-dependency reading gains strength and the constraint reclassifies toward rope. If evidence is ambiguous, mandatrophy remains open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_intentionality_and_coordination, empirical, 'Whether QWERTY standardization was strategically engineered or emergent path-dependent coordination.').

omega_variable(
    identity_lock_in_typist_switching_costs,
    'Is the identity-locking of professional typists to QWERTY structural (ergonomic muscle memory, job certification, employment history) or internalized (belief that the layout is inevitable, normalization of the constraint)?',
    'Natural experiment: when keyboard layout switches became technically possible (computers, 1970s+), did professional typists actually switch to superior layouts despite switching costs? Did retraining barriers persist after the institutional lock-in (cartel control) was dissolved? Did cognitive/identity barriers remain?',
    'If typists readily adopted superior layouts once institutional barriers fell, identity-locking was structural (dependent on cartel enforcement) and is correctly measured as moderately high exit-friction. If typists remained locked even after institutional barriers fell, the constraint carried internalized suppression that the authored suppression metric may underestimate—the effective suppression was higher than structural barriers alone would predict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_typist_switching_costs, empirical, 'The mechanism of typist lock-in: structural cartel control vs. internalized identity fusion.').

omega_variable(
    sibling_reading_foreclosure_or_coexistence,
    'Does the strategic lock-in reading logically foreclose the path-dependency reading, or do they coexist as live competing hypotheses?',
    'Logical analysis: the strategic reading claims intentional cartel action prevented alternatives from reaching critical mass. The path-dependency reading claims QWERTY persistence is path-dependent accident without strategic mechanism. These are not logically contradictory—both could be true (accident-initial, then strategic-maintenance), or one could be true. The question is whether archival evidence of cartel coordination RULES OUT the path-dependency narrative entirely, or whether path-dependency remains a live alternative explanation for aspects of persistence.',
    'If cartel-coordination evidence is definitive, the readings foreclose each other and only one constraint type computes (snare or tangled_rope vs. mountain/rope). If evidence is ambiguous or if path-dependency explains early persistence and strategy explains later maintenance, both readings coexist and should compile as separate constraints linked via network.affects_constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_or_coexistence, conceptual, 'The logical relationship between strategic lock-in and path-dependent persistence explanations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1890, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1890, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1890, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t1890, observed).
narrative_ontology:measurement(qwer_tr_t1905, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1905, 0.15).
narrative_ontology:measurement_basis(qwer_tr_t1905, observed).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement_basis(qwer_tr_t1920, observed).
narrative_ontology:measurement(qwer_tr_t1935, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1935, 0.36).
narrative_ontology:measurement_basis(qwer_tr_t1935, observed).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1950, 0.4).
narrative_ontology:measurement_basis(qwer_tr_t1950, observed).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1960, 0.42).
narrative_ontology:measurement_basis(qwer_tr_t1960, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1890, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1890, 0.15).
narrative_ontology:measurement_basis(qwer_be_t1890, observed).
narrative_ontology:measurement(qwer_be_t1905, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1905, 0.38).
narrative_ontology:measurement_basis(qwer_be_t1905, observed).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1920, 0.54).
narrative_ontology:measurement_basis(qwer_be_t1920, observed).
narrative_ontology:measurement(qwer_be_t1935, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1935, 0.62).
narrative_ontology:measurement_basis(qwer_be_t1935, observed).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1950, 0.66).
narrative_ontology:measurement_basis(qwer_be_t1950, observed).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1960, 0.68).
narrative_ontology:measurement_basis(qwer_be_t1960, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1890, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1890, 0.2).
narrative_ontology:measurement_basis(qwer_su_t1890, observed).
narrative_ontology:measurement(qwer_su_t1905, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1905, 0.45).
narrative_ontology:measurement_basis(qwer_su_t1905, observed).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement_basis(qwer_su_t1920, observed).
narrative_ontology:measurement(qwer_su_t1935, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1935, 0.66).
narrative_ontology:measurement_basis(qwer_su_t1935, observed).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1950, 0.69).
narrative_ontology:measurement_basis(qwer_su_t1950, observed).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1960, 0.71).
narrative_ontology:measurement_basis(qwer_su_t1960, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.12).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence_inevitability kernel has two structurally distinct constraint readings: (1) path_dependency_reading attributes persistence to historical accident and self-reinforcing network effects without identifiable strategic beneficiaries, classifying as rope or mountain depending on naturalness claims; (2) strategic_lock_in_reading (this story) attributes persistence to manufacturer-engineered lock-in via cartel standardization and training-pathway control, classifying as tangled_rope with identifiable beneficiaries and victims. The readings share the same empirical referent (QWERTY's actual persistence) but differ on mechanism and beneficiary structure. Evidence discriminating them includes trade-association records, patent-pool agreements, and the trajectory of alternative-layout suppression. Both readings must be authored to model the full contest; they are not the same constraint viewed from different seats, but rather different structural claims about the same persistent phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_inevitability__strategic_lock_in_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
