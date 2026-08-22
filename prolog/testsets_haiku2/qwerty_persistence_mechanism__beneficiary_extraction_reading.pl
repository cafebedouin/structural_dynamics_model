% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Persistence via Beneficiary Extraction
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This is the BENEFICIARY-EXTRACTION READING of the QWERTY persistence
 *   kernel. It frames keyboard standardization as an active, enforced
 *   extraction mechanism: Remington, Union Typewriter, and incumbent typing
 *   schools benefited enormously from QWERTY lock-in and actively maintained
 *   it—funding typing curricula, lobbying against layout alternatives, and
 *   raising switching costs for both users and innovators. The reading does
 *   not claim QWERTY was chosen cynically; it was genuinely coordinated early
 *   on. The reading DOES claim that after the coordination problem was solved
 *   (by 1920), the incumbents maintained the standard not because continued
 *   standardization was needed but because breaking the standard threatened
 *   their sunk capital and competitive position. This reading identifies
 *   beneficiaries, declares victims, and models active suppression of
 *   alternatives.
 *
 * KEY AGENTS:
 *   - Remington and Union Typewriter (institutional, agenda-setter): captured QWERTY standardization, invested massive capital in QWERTY-specific tooling, and defended the standard as their competitive moat.
 *   - Incumbent typing instruction schools (organized, beneficiary/agenda-setter): built curricula and reputation around QWERTY mastery; resisted alternatives because any shift threatened institutional survival.
 *   - Individual typists (powerless, identity-locked payer): learned QWERTY in school, internalized it as professional identity, and faced high personal switching costs despite technical availability of faster layouts.
 *   - Dvorak and layout innovators (moderate power, trapped): developed technically superior alternatives but were systematically excluded from market pathways (no manufacturer support, no school adoption, no employer funding).
 *   - Technical researchers and ergonomists (analytical observers): documented QWERTY inferiority but remained marginal because incumbents controlled industry standard-setting.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.72).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence via Beneficiary Extraction").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '39f94634-bda7-4918-8b9a-7ca2adb5789f').
narrative_ontology:cs_kernel_codification('39f94634-bda7-4918-8b9a-7ca2adb5789f', formalized).
narrative_ontology:cs_authority_grounding('39f94634-bda7-4918-8b9a-7ca2adb5789f', extraction).
narrative_ontology:cs_reading_relation('39f94634-bda7-4918-8b9a-7ca2adb5789f', qwerty_persistence_mechanism__lock_in_reading, influences).
narrative_ontology:cs_reading_relation('39f94634-bda7-4918-8b9a-7ca2adb5789f', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_axiom('39f94634-bda7-4918-8b9a-7ca2adb5789f', foundational, incumbent_intentional_suppression_of_alternatives).
narrative_ontology:cs_axiom_status(incumbent_intentional_suppression_of_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('39f94634-bda7-4918-8b9a-7ca2adb5789f', incumbent_intentional_suppression_of_alternatives, empirically_contingent).
narrative_ontology:cs_axiom('39f94634-bda7-4918-8b9a-7ca2adb5789f', foundational, beneficiary_extraction_via_switching_costs).
narrative_ontology:cs_axiom_status(beneficiary_extraction_via_switching_costs, holdable).
narrative_ontology:cs_axiom_grounding('39f94634-bda7-4918-8b9a-7ca2adb5789f', beneficiary_extraction_via_switching_costs, deontological).
narrative_ontology:cs_reference_frame('39f94634-bda7-4918-8b9a-7ca2adb5789f', qwerty_as_defended_monopoly_rent).
narrative_ontology:cs_drift_state('39f94634-bda7-4918-8b9a-7ca2adb5789f', post_dvorak_technical_superiority_establishment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('39f94634-bda7-4918-8b9a-7ca2adb5789f', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_instruction_schools).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, users_trapped_in_qwerty_layout).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, competing_keyboard_layout_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_managers_and_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Remington, Union Typewriter, and other dominant manufacturers had massive capital invested in QWERTY-specific tooling, parts inventories, and trained workforce. They funded typing schools, standardized QWERTY curricula, and actively lobbied against alternative layouts. Their extractive interest: maintaining QWERTY lock-in preserved the value of their sunk assets and prevented competitor entry via layout innovation. They could have retooled to Dvorak or other layouts, but the cost exceeded the benefit because QWERTY standardization was worth more to them as a defended monopoly than as a neutral choice.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers, agenda_setter,
    institutional, generational, arbitrage, national).

% Business colleges, secretarial schools, and institutional typing programs had curricula, teacher training, certification standards, and institutional reputation built on QWERTY mastery. Any shift to a rival layout threatened their competitive position, required curriculum revision, and undermined their brand claim to teach 'professional typing.' They actively resisted layout alternatives through accreditation bodies, industry associations, and employer partnerships. Their benefit: QWERTY standardization locked in their curriculum value and made them the gatekeepers of labor supply.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_instruction_schools, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_instruction_schools, agenda_setter).

% Individual typists—secretaries, office workers, journalists—learned QWERTY in schools, through workplace training, or through practice. Their hands and muscle memory became bound to QWERTY; they developed professional identity as 'QWERTY typists.' Switching to Dvorak or another layout required unlearning deeply ingrained motor skills (estimated 20–40 hours of reduced productivity) and retraining—a high personal cost that would be borne entirely by them while the industry remained QWERTY-standardized. Their benefit from switching (faster typing, fewer repetitive-strain injuries) would accrue only to them individually, not shared across the labor pool. The coordination problem was asymmetric: everyone else staying on QWERTY made switching irrational for any individual. Their identity-lock meant that even when faster alternatives became known, they experienced switching as threatening their professional competence and self-image.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, users_trapped_in_qwerty_layout, payer,
    powerless, biographical, identity_locked, national).

% August Dvorak and other layout designers developed technically superior arrangements (faster typing, less fatigue, better ergonomics). Dvorak filed patents in 1936 and conducted studies showing 10-15% speed improvements. However, Dvorak and his successors could not gain market traction because: (1) manufacturers refused to build alternative-layout machines (no capital investment without demand); (2) schools refused to teach alternatives (locked into QWERTY curricula); (3) users could not see benefit in learning an isolated layout (network effects). Their technical innovation was defeated not by inferior design but by incumbents' control of production and distribution channels. They were trapped because breaking into the market required simultaneous adoption across multiple layers (manufacturers, schools, users)—a coordination problem they could not solve from outside the incumbent coalition.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, competing_keyboard_layout_innovators, payer,
    moderate, biographical, trapped, national).

% Large employers (banks, insurance companies, government offices, newspapers) benefited from a standardized, abundant pool of QWERTY-trained typists they could hire without retraining. They preferred standardization (even suboptimal) to fragmentation, and so resisted funding alternative-layout training or purchasing equipment that would fragment their labor supply. They formed a secondary beneficiary coalition: by refusing to hire Dvorak-trained typists or purchase Dvorak machines, they reinforced the incumbents' lock-in mechanism.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_managers_and_employers, beneficiary,
    powerful, biographical, constrained, national).

% Ergonomists, efficiency researchers, and technical journalists (including academic studies from MIT, Carnegie Tech, and industrial engineers) documented that Dvorak and other layouts were faster, less fatiguing, and biomechanically superior to QWERTY. Their research was technically sound but remained marginal in industry policy because manufacturers and schools had no incentive to fund adoption research; the constraint's suppression of alternatives meant technical evidence had minimal influence on standardization decisions. Their observations confirmed the extraction mechanism: QWERTY persisted not because it was optimal but because incumbents had invested in defending it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typing_speed_researchers, observer,
    analytical, biographical, analytical, national).

% Standards bodies (typewriter manufacturers' associations, business education councils, early computing standards committees) that might have mandated layout optimization, opened competition, or required disclosure of alternatives were either directly influenced by manufacturer interests or deemed keyboard design 'market-driven' rather than subject to regulation. Their potential role—counterbalancing incumbent influence—was preempted or captured. They were excluded from the extraction mechanism's benefits and so lacked motivation to intervene.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, regulatory_bodies_and_standards_committees, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single universal keyboard layout so all typists can operate any machine and all machines can be built with one standard interface, reducing friction in labor markets (workers can move jobs without retraining; employers can hire any typist; manufacturers can sell machines to any trained operator). QWERTY solved the pre-standardization problem of keyboard anarchy (different machines used different arrangements).
% TRANSFER_FUNCTION: Extracts switching costs from users and innovators, transferring the value of standardization to incumbent manufacturers (Remington, Union) and typing instruction institutions. Every typist who learned QWERTY and could not affordably switch to a faster layout bore a cost (opportunity cost of slower typing, health costs from repetitive strain); every innovator with a superior layout bore a cost (zero market access). The incumbents captured the standardization rent: their sunk QWERTY investments appreciated in value; their labor supply remained locked in; their competitive position became unassailable.
% ABSENT_VOICES: Layout innovators (Dvorak descendants, post-1970s ergonomic researchers), workers in alternative-layout communities who would have testified that switching is possible when barriers drop, regulatory bodies that might have intervened to mandate layout competition or disclosure, and the many typists who experienced repeated-strain injury and would have preferred faster layouts but never learned they existed as realistic options. Their absence from standards discussions meant the constraint's extraction mechanism operated unopposed.
% DISAPPEARANCE_RATIONALE: If the incumbents ceased active suppression (manufacturers adopted Dvorak or remained agnostic, typing schools taught multiple layouts, standards committees opened to alternatives), the constraint would dissolve. New machines would ship with switchable layouts; schools would diversify curricula; typists would gradually migrate to faster layouts; the labor market would reorganize around technical merit. The persistence of QWERTY depends on ongoing incumbent action—specifically, on the manufacturers' and schools' refusal to invest in alternative layouts despite technical superiority. Remove that active suppression and the market reorganizes; the constraint is not self-sustaining.
% FOUNDING_PROBLEM: Early typewriter designs (1870s–1880s) used different key arrangements (DHIATENSOR, Sholes variants, others). A business operator or typist moving jobs had to learn each machine's unique layout. This fragmentation made labor markets inefficient and made capital investment in typewriter manufacturing risky. Standardization on a single layout solved this coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: Technology historian David (1985, 1986) and economist Arthur (1989) established through historical analysis that the original coordination problem was solved by 1895–1920. QWERTY achieved critical mass; labor markets stabilized; manufacturers could rely on a trained typist pool; the efficiency gains from standardization were realized by 1920. Subsequent persistence is attributed by these outside scholars to lock-in economics and incumbent rent-seeking, not to ongoing coordination failure. Typing instruction became decoupled from innovation: schools locked curricula into QWERTY; employers refused to hire Dvorak-trained typists; the coordination benefit became a sunk asset that incumbents defended, not a living problem they solved. The founders' stated rationale (coordination) has been explicitly superseded by the beneficiary-extraction account documented by scholars outside the incumbent group.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 1895–1920 as manufacturers institutionalized QWERTY and began active suppression (Dvorak development ~1930s, rejected by industry). It stabilizes at high levels 1950–1980 because the constraint becomes self-reinforcing: no alternative can break in because no manufacturer invests, no school teaches it, and users face identity-lock. Suppression rises correspondingly: the constraint's persistence increasingly depends on active defense (standards committees captured, Dvorak lobbying rebuffed, academic evidence sidelined). Theater rises gradually: the original coordination rationale becomes increasingly decoupled from actual operation—by 1950, the rhetoric is 'QWERTY is the standard and we all use it' rather than 'QWERTY solves the coordination problem,' but the machinery of enforcement remains active. The beneficiary-extraction reading is structural: if QWERTY had ceased to be beneficial to incumbents while remaining coordinated (hypothetically), they would have switched to optimize. They did not, suggesting protection of sunk assets, not pure coordination, drove persistence.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent-manufacturer seat: QWERTY is a valuable standard they built and maintain; alternatives are inefficient and would disrupt trained workforces; the persistence is coordination-justified. From the typist seat: QWERTY is a lock that they paid to learn and cannot affordably exit; faster alternatives exist but are invisible because market mechanisms are broken; the persistence is imposed, not chosen. From the innovator seat: better designs lose because they face insurmountable adoption barriers, not because they are technically inferior; the market is rigged. The engine computes these divergences from power (institutional vs. powerless), exit options (arbitrage vs. identity-locked vs. trapped), and beneficiary/victim declarations. The constraint's classification should differ across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent manufacturers and typing schools are structural beneficiaries: they enjoy arbitrage-grade exit (they COULD switch to Dvorak and retool, but the cost exceeds their benefit because QWERTY is worth more to them as a defended standard than as a neutral choice). Their power is institutional and their time-horizon long (generational). Individual typists are high-d targets: they are trapped (powerless) and identity-locked (their motor skills are QWERTY-specific; they have paid unrecoverable training costs). Competing innovators are trapped: they cannot enter the market because manufacturers and schools (the incumbents' allied institutions) refuse to adopt alternatives. The effective extraction flows FROM the trapped typists and excluded innovators TO the incumbents who defended QWERTY as a protected asset. The measure of that directionality is the gap between what typists would have gained on a faster layout vs. what they actually earned on QWERTY—a surplus appropriated by the incumbents' control of standardization.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled Rope type requires: (1) genuine coordination function—it does (standardization solves trainability); (2) asymmetric extraction—it does (users and innovators pay switching costs; incumbents collect standardization rents); (3) active enforcement—it does (manufacturers lobby, schools control curricula, standards committees exclude alternatives, Dvorak adoption is suppressed). All three gates are satisfied. The constraint is NOT a Rope because the symmetric-benefit story (everyone coordinated and benefits equally) breaks down once the founding problem is solved; after 1920, the coordination value decouples from the extraction value, and enforcement is required to defend only the beneficiaries' position, not the coordination itself. It is NOT a Snare because the coordination function remains real (standardization does enable labor-pool mobility); the constraint serves both functions simultaneously. This is exactly the Tangled Rope structure: genuine coordination wrapped in extraction, defended by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_vs_natural_coordination,
    'Did QWERTY persist primarily because active incumbent suppression made alternatives impossible to enter, or because QWERTY was legitimately adequate and alternatives failed through fair market competition?',
    'Counterfactual analysis: if manufacturers had adopted Dvorak in 1940 (post-David studies) and invested equally, would typist adoption have followed? Historical evidence from industries where no incumbent dominated (early telephone, early aviation standards) shows rapid adoption of superior designs when competition is open; QWERTY shows persistent stagnation despite technical superiority of alternatives—this pattern divergence supports the suppression reading.',
    'If suppression was primary: this is Tangled Rope with active extraction. If competition was fair: this is Rope or even Mountain (QWERTY persists because it''s adequate). The classification swings on this axis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_natural_coordination, empirical, 'Whether QWERTY persistence is driven by active incumbent suppression or natural market equilibrium.').

omega_variable(
    founding_problem_death,
    'After what date did the original coordination problem (labor-pool trainability) become solved, and did the incumbents'' rationale for maintaining QWERTY shift from coordination to rent protection?',
    'Textual analysis of manufacturer advertising, trade journal rhetoric, and standards committee discussions 1890–1950. Early messaging emphasizes coordination and trainability; post-1920 messaging shifts to ''QWERTY is the standard'' (naturalization) while rarely defending it on coordination grounds. A clear rhetorical pivot from functional to tautological justification would support the shift.',
    'If the founding problem died ~1920 but persistence continued: it confirms mandatrophy_resolved and supports Tangled Rope classification. If the founding problem remained live: the constraint might be a Rope (coordination maintained for its continued value).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_death, empirical, 'The historical moment when QWERTY transitioned from necessary coordination to defended extraction.').

omega_variable(
    beneficiary_capture_of_standards,
    'Did the incumbents actively capture or shape standards committees and professional bodies, or did these bodies independently adopt QWERTY through democratic deliberation?',
    'Archival study of standards-committee membership, funding sources, voting records, and rejected proposals 1920–1980. Direct evidence of Remington/Union Typewriter personnel on committees, funding of standards bodies, or rejection of competing proposals would support capture; absence would support independent adoption.',
    'If capture is documented: suppression is institutional, not just market-based—the constraint requires active (political) enforcement. If independent: QWERTY persists through coordination, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_standards, empirical, 'Whether keyboard standardization was captured by incumbents or arose through open deliberation.').

omega_variable(
    alternative_reading_assignment,
    'Is this constraint truly a Tangled Rope (hybrid coordination/extraction), or is it better classified as a pure Snare (extraction disguised as coordination)?',
    'Natural experiment: countries where no manufacturer monopolized (e.g., socialist states with multiple typewriter producers, or postwar periods with fragmented manufacture). Did they standardize QWERTY or fragment? Fragmentation supports Snare (lock-in persists even without incumbent defense); standardization on QWERTY supports Tangled Rope (coordination is genuinely valued).',
    'Reclassification between Tangled Rope and Snare, with implications for whether fixing it requires (1) unlocking path-dependency (Tangled Rope fix) or (2) breaking incumbent control (Snare fix).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_assignment, conceptual, 'Whether the constraint''s core mechanism is coordination-plus-extraction or pure extraction disguised as coordination.').

omega_variable(
    identity_lock_internalization,
    'Is typist attachment to QWERTY primarily structural (no viable exit) or internalized (typists believe QWERTY is natural/best)?',
    'Survey or interview evidence: do typists report QWERTY preference as performance-based or as identity/habit? Do they resist learning Dvorak as too costly, or as ''wrong''? Early adopter communities (e.g., programming communities post-1970) that voluntarily adopted Dvorak would show externalized exit (people CAN switch when barriers drop); mass-market typists'' resistance would show internalization. Post-digital era (computer keyboards, no manufacturer lock-in), do typists adopt Dvorak? Yes = structural barrier was primary; No = identity-lock was primary.',
    'If internalized: the constraint''s suppression is higher than the structural measure suggests; users carry it with them even after exit. If structural: fixing the constraint requires only removing manufacturers'' enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether typists'' loyalty to QWERTY is structural or identity-rooted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 1873, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t1873, projected).
narrative_ontology:measurement(qwer_tr_t1895, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1895, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t1895, observed).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement_basis(qwer_tr_t1920, observed).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1950, 0.38).
narrative_ontology:measurement_basis(qwer_tr_t1950, observed).
narrative_ontology:measurement(qwer_tr_t1970, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement_basis(qwer_tr_t1970, observed).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1980, 0.41).
narrative_ontology:measurement_basis(qwer_tr_t1980, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1873, 0.15).
narrative_ontology:measurement_basis(qwer_be_t1873, projected).
narrative_ontology:measurement(qwer_be_t1895, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1895, 0.32).
narrative_ontology:measurement_basis(qwer_be_t1895, observed).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement_basis(qwer_be_t1920, observed).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1950, 0.64).
narrative_ontology:measurement_basis(qwer_be_t1950, observed).
narrative_ontology:measurement(qwer_be_t1970, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1970, 0.66).
narrative_ontology:measurement_basis(qwer_be_t1970, observed).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement_basis(qwer_be_t1980, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1873, 0.08).
narrative_ontology:measurement_basis(qwer_su_t1873, projected).
narrative_ontology:measurement(qwer_su_t1895, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1895, 0.35).
narrative_ontology:measurement_basis(qwer_su_t1895, observed).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement_basis(qwer_su_t1920, observed).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement_basis(qwer_su_t1950, observed).
narrative_ontology:measurement(qwer_su_t1970, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1970, 0.71).
narrative_ontology:measurement_basis(qwer_su_t1970, observed).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement_basis(qwer_su_t1980, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.22).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% QWERTY persistence kernel decomposes into three readings with different ε and beneficiary structures. This reading (beneficiary_extraction_reading) models active incumbent suppression of alternatives; ε=0.68 reflects extraction via switching costs. The lock_in_reading models path-dependent coordination failure without beneficiary intentionality; ε would be lower (~0.45), suppression would reflect system dynamics rather than enforcement. The naturalization_reading models QWERTY as genuinely adequate; ε would be negligible (~0.15), accessibility_collapse high (~0.85), claiming natural selection, not designed extraction. All three readings share the kernel (keyboard standardization) but differ on mechanisms, beneficiaries, and classified type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, powerless, 0.82).
constraint_indexing:directionality_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
