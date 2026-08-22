% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Persistence via Incumbent Preservation Defense
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This constraint instantiates the incumbent-preservation reading of the
 *   QWERTY persistence kernel. The reading asserts that QWERTY's dominance
 *   persists not primarily because alternatives fail to reach adoption
 *   critical mass (the lapsed-alternatives sibling reading), but because
 *   manufacturers, trained typists, and training institutions actively defend
 *   QWERTY against superior alternatives in order to protect sunk capital
 *   investments. The founding problem (mechanical jam-up on early
 *   typewriters) was solved by 1920; QWERTY persisted after that by
 *   deliberate incumbent coordination, not by neutral network effects. This
 *   reading yields a Tangled Rope constraint: real coordination value exists
 *   (typists benefit from standardization), but extraction accompanies it
 *   (efficiency-seekers and alternative-adopters bear opportunity costs;
 *   innovators are excluded). The suppression trajectory (rising from 0.18 to
 *   0.71) models the escalation of defensive incumbent action: early
 *   enforcement was cheap (standards bodies, training dominance). By
 *   mid-century, as alternatives like Dvorak gained documented evidence of
 *   superiority, suppression intensified (patent strategies,
 *   certified-testing barriers, deliberate omission from standards
 *   committees). Theater rises (0 to 0.42) as the coordination justification
 *   persists even after the founding problem is solved — the constraint's
 *   legitimacy increasingly rests on the narrative of standardization rather
 *   than on any remaining technical necessity.
 *
 * KEY AGENTS:
 *   - typewriter_manufacturers (Remington, Olivetti, Underwood): institutional agenda-setters; trapped in QWERTY capital; actively defend via standards bodies, tooling lock-in, and training-pipeline capture
 *   - trained_typists_incumbent_cohort: organized beneficiaries and secondary payers; benefit from standardization and skill value; pay opportunity cost via locked-in inefficiency
 *   - typing_training_institutions: organized beneficiaries; locked into QWERTY curricula; resist alternatives to avoid retraining and certification disruption
 *   - alternative_adopters (Dvorak proponents, regional ergonomic layout communities): payers and excluded agents; bear incompatibility costs; fail to achieve critical-mass adoption despite technical superiority
 *   - efficiency_seeking_users: powerless, identity-locked payers; suffer RSI and fatigue; cannot exit because employment and training ecosystems assume QWERTY
 *   - standards_bodies (ISO, ANSI): nominally neutral but structurally vendor-captured; codify QWERTY as international standard; create high barriers to alternative certification
 *   - innovators (Dvorak, ergonomic researchers): excluded from standards process; bear R&D costs with no market outcome; would have dominated an alternative ecosystem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.71).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Persistence via Incumbent Preservation Defense").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, 'ac949359-275d-49a7-8fc7-1f8e8f8f357a').
narrative_ontology:cs_kernel_codification('ac949359-275d-49a7-8fc7-1f8e8f8f357a', distributed).
narrative_ontology:cs_authority_grounding('ac949359-275d-49a7-8fc7-1f8e8f8f357a', extraction).
narrative_ontology:cs_interpretation_layer_present('ac949359-275d-49a7-8fc7-1f8e8f8f357a').
narrative_ontology:cs_reading_relation('ac949359-275d-49a7-8fc7-1f8e8f8f357a', qwerty_persistence__lapsed_alternatives_reading, influences).
narrative_ontology:cs_axiom('ac949359-275d-49a7-8fc7-1f8e8f8f357a', foundational, incumbent_coordination_necessary_for_persistence).
narrative_ontology:cs_axiom_status(incumbent_coordination_necessary_for_persistence, holdable).
narrative_ontology:cs_axiom_grounding('ac949359-275d-49a7-8fc7-1f8e8f8f357a', incumbent_coordination_necessary_for_persistence, empirically_contingent).
narrative_ontology:cs_axiom('ac949359-275d-49a7-8fc7-1f8e8f8f357a', foundational, alternatives_suppressed_not_naturally_lapsed).
narrative_ontology:cs_axiom_status(alternatives_suppressed_not_naturally_lapsed, holdable).
narrative_ontology:cs_axiom_grounding('ac949359-275d-49a7-8fc7-1f8e8f8f357a', alternatives_suppressed_not_naturally_lapsed, empirically_contingent).
narrative_ontology:cs_reference_frame('ac949359-275d-49a7-8fc7-1f8e8f8f357a', qwerty_as_defended_incumbency).
narrative_ontology:cs_drift_state('ac949359-275d-49a7-8fc7-1f8e8f8f357a', contemporary_post_mechanical_constraint, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ac949359-275d-49a7-8fc7-1f8e8f8f357a', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typists_incumbent_cohort).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, innovators_proposing_superior_layouts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, standards_bodies_vendor_captured).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, trained_typists_incumbent_cohort).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominant manufacturers (Remington, Olivetti, Underwood) invested enormous capital in tooling, supply chains, and training ecosystems optimized for QWERTY. They actively fund standards bodies, lobby governments, and coordinate with training institutions to prevent alternative layouts from gaining market share. Their exit from QWERTY would strand their capital investments and undermine their competitive moat against upstart rivals with different layout bets. They frame QWERTY as the universal standard and defend it through technical committees, patent strategies, and control of keyboard manufacturing.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typewriter_manufacturers, agenda_setter,
    institutional, generational, trapped, global).

% Millions of typists trained on QWERTY machines during the early 20th century. They benefit from the network effect: every new typewriter shipped uses QWERTY, so their skill remains universally valuable. They also pay an opportunity cost: they are locked into QWERTY muscle memory, cannot easily adopt more efficient layouts even if personal equipment could support them, and bear the productivity loss from QWERTY's deliberate finger-speed reduction (designed to prevent mechanical jam-ups). Their organized voice through secretarial unions and professional associations defends the standard they invested in learning.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typists_incumbent_cohort, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, trained_typists_incumbent_cohort, payer).

% Schools, business colleges, and stenotype academies built entire curricula around QWERTY instruction. Their instructors were trained on QWERTY; their textbooks, practice regimens, and certification standards all assume QWERTY. They benefit from standardization: students trained on a universal layout are more readily hired. Switching to an alternative layout would require retraining instructors, rewriting materials, and accepting a period where their graduates are not immediately hireable on the dominant standard. They actively oppose layout changes through educational policy advocacy.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    organized, generational, constrained, national).

% Users and small manufacturers who recognized QWERTY's inefficiency and proposed or adopted alternatives (Dvorak in 1936, various ergonomic layouts in the 1980s–2000s). They bear the cost of incompatibility: their machines cannot interoperate with the dominant standard, they cannot easily find trained operators, and they face market rejection because buyers default to QWERTY familiarity. Their attempts to build alternative ecosystems fail due to network effects amplified by incumbent coordination. They are excluded from the dominant training pipeline and face constant pressure to conform.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_adopters, payer,
    moderate, biographical, trapped, regional).

% Typists and data-entry workers who suffer repetitive strain injury, fatigue, and reduced productivity from QWERTY's deliberately sub-optimal letter placement. They would benefit materially from alternatives like Dvorak (20–35% faster, lower error rate, reduced RSI risk documented in controlled studies). They are locked into QWERTY because the training they received, the machines they access, and the jobs they compete for all assume QWERTY. Personal switching (learning Dvorak at home) is possible but creates a dual-competency burden and career risk: employers assume QWERTY-only, and hiring tests are QWERTY-standardized.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users, payer,
    powerless, biographical, identity_locked, global).

% Researchers and engineers who designed demonstrably superior keyboard layouts (August Dvorak, Liliana Dvorak, and later ergonomic researchers) but could not achieve market adoption because the coordination problem favors the incumbent. Their innovations are technically sound but structurally excluded by the QWERTY network effect defended by incumbents. They bear the opportunity cost of years of research that produced no commercial outcome because adoption required overcoming incumbent opposition and network lock-in.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, innovators_proposing_superior_layouts, excluded,
    powerless, biographical, trapped, local).

% Technical standards organizations (ISO, ANSI committees on keyboard layout) that codified QWERTY as the international standard. They function as deliberative bodies but are structured to ensure incumbent manufacturers dominate membership and voting. They certify QWERTY as the official standard and require expensive compliance testing for any alternative, creating a high barrier to entry for layout changes. Their stated mandate is standardization; their effective function is standardization *of the incumbent choice*.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, standards_bodies_vendor_captured, agenda_setter,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, standards_bodies_vendor_captured, beneficiary).

% Regulatory bodies that rarely intervened in QWERTY standardization during the typewriter era (1950s–1980s), viewing it as a settled technical matter outside competition law. They later became analytical observers when keyboard standards became visible in the early computing era, but by then the lock-in was structurally complete. They take historical testimony and economic analysis of path dependence but have limited retroactive remedial authority.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, competition_authorities_historical, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, typewriter_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solving the coordination problem of keyboard layout standardization: typists need machines with familiar layouts, manufacturers need to know which layout to produce, training institutions need to know what to teach. A single standard solves this mutual dependency — once QWERTY reached critical mass, that coordination value is real and substantial.
% TRANSFER_FUNCTION: Moves the coordination surplus and the opportunity gains from layout efficiency from the entire user population to the beneficiary set (manufacturers, incumbent-trained typists, training institutions). Specifically: manufacturers retain market power and capital-asset value; incumbent typists retain their human capital from prior training and avoid retraining costs; training institutions avoid curriculum redesign. Alternative-adopters and efficiency-seekers transfer their potential gains (faster typing, fewer errors, reduced injury) to the incumbent beneficiaries via lock-in and exclusion.
% ABSENT_VOICES: Typists who would have been trained on Dvorak or ergonomic layouts if born in an alternative ecosystem (counterfactual set — not present by definition). Efficiency-seeking users who bear RSI and fatigue costs; their organized advocacy is structurally muted because complaining about a 'standard' is framed as non-technical noise rather than a safety/health claim. Innovators and alternative-layout researchers whose work was excluded from the standards process entirely. Future-born typists locked into a century-old layout due to path dependence (the youngest affected cohorts are born *into* the constraint, not choosing it).
% DISAPPEARANCE_RATIONALE: If QWERTY enforcement and incumbent defense mechanisms vanished overnight, the typewriter and early computing industries would have reorganized within a generation: manufacturers would hedge bets across layouts, training institutions would diversify curricula, and Dvorak and ergonomic alternatives would have achieved competitive adoption. The constraint's persistence is not a law of physics; it is a sustained incumbent choice to block alternatives because alternatives threaten sunk-capital value. Remove the defense mechanisms and alternatives reappear as live options within 10–20 years.
% FOUNDING_PROBLEM: Mechanical typewriter design created a real coordination problem: early machines had mechanical jams when typists exceeded certain finger-speed thresholds on common letter sequences. A standard layout was needed to reduce collisions. QWERTY was chosen (likely for reasons of patent control by Sholes and Remington, not pure ergonomic optimization) and became standardized through manufacturing dominance and training pipeline lock-in.
% FOUNDING_PROBLEM_CORROBORATION: The mechanical jam-up problem was solved by 1920 with improved mechanics (segmented typebars, basket-shift systems, better springs). By mid-century, electric machines eliminated the constraint entirely. Yet QWERTY persisted. This is corroborated by engineering histories (David, 1985; Arthur, 1989; Liebowitz & Margolis, 1990) and testimony from alternative-layout researchers who documented that the technical constraint had disappeared but the standardization constraint remained. No corroboration from incumbent manufacturers' historical accounts — they frame QWERTY as a natural standard, not a defended choice.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (founding problem solved, QWERTY purely coordinating) to 0.68 (incumbent defense overhead + efficiency lock-in + opportunity transfer) because the incumbents shift from operating a coordination standard to defending a sunk-asset moat. Suppression rises faster (0.18 to 0.71) because defending QWERTY against documented alternatives (Dvorak: 20–35% faster, lower error) requires active exclusion, not just standardization. Theater accelerates sharply (0 to 0.42 by 1960) because the mechanical constraint ceased to exist by 1920; by 1960 the primary enforcement activity is defending the narrative ('QWERTY is the universal standard') rather than solving any remaining coordination problem. The suppression_requirement trajectory models the cost of holding the constraint in place: before 1920, enforcement is cheap (naturally dominant due to technical constraints). By 1960, alternatives are demonstrably superior, so suppression must intensify (certified testing, standards exclusion, reputational discouragement of alternatives). The measurement series shares one time grid: every metric is authored at every examined time point (1873, 1920, 1940, 1960, 1975, 1985), enabling legitimate temporal analysis.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiary seats compute the constraint as Rope (coordination needed, alternatives are fringe); payer seats compute it as Snare or Tangled Rope (coordination covers enforced extraction). The engine's per-seat classification from the structural data reveals the gap without reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers: powerful, trapped (their capital is QWERTY-specific), beneficiary. Directionality leans beneficiary (d near 0.2): they control the constraint and collect the surplus. Trained typists: organized, constrained (retraining cost, job market risk), beneficiary + secondary payer. Directionality ~0.45 (symmetric): they benefit from standardization but pay an ongoing opportunity cost (20–35% productivity loss) they cannot escape. Efficiency-seekers: powerless, identity-locked (employer and job-market assumptions), payers. Directionality near 1.0 (full target): they bear the extraction and have no exit. Alternative-adopters: moderate power, trapped (incompatibility cost, network effects amplified by incumbent action), payers. Directionality ~0.85 (near-target): they attempted to compete but were excluded by incumbent coordination. Incumbents' suppression actions (standards exclusion, training-pipeline capture) are the mechanism that transforms a natural coordination problem into an extractive lock-in — the authoring reflects this by modeling rising suppression_requirement (the cost of defending QWERTY rises as alternatives become technically superior).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical jam-up) is DEAD as of 1920. Yet QWERTY persists and extractiveness rises post-1920 rather than falling. The constraint exhibits mandatrophy: the problem it was built to solve is gone, but incumbent defense and capital lock-in keep it in place. This is detected by (founding_problem_status = dead) paired with (base_extractiveness rising, theater_ratio rising, suppression_requirement rising). The classification remains Tangled Rope (real coordination value + extraction) rather than collapsing to Piton because the constraint has not yet atrophied into pure theater — enforcement machinery is still functional and costly, not yet reduced to ceremonial maintenance. The constraint sits at the Tangled Rope → Piton boundary: if theater continues rising and extractiveness plateaus (enforcement costs match extraction benefit), the classification would migrate to Piton in later periods (post-computing era, 1990+).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incumbent_vs_neutral_network_effects,
    'Is QWERTY''s persistence attributable to incumbent defensive actions, or would it persist as a natural network-effect equilibrium even without incumbent coordination?',
    'Counterfactual analysis: if manufacturers had been required to maintain compatibility with multiple keyboard layouts (regulatory mandate opening standards), would Dvorak or ergonomic alternatives have achieved market adoption? Or would QWERTY still dominate despite open competition? Historical case studies of standardization in other domains (e.g., railroad gauges, electrical grid voltage) where multiple competitors operated simultaneously can provide evidence of whether incumbent coordination is necessary to maintain dominance.',
    'If incumbent coordination is necessary, the constraint is Tangled Rope with real extraction. If QWERTY would dominate through neutral network effects even without active defense, the constraint reclassifies to Rope (coordination only). This is the core disagreement between the incumbent_preservation_reading and the lapsed_alternatives_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_vs_neutral_network_effects, empirical, 'Whether QWERTY''s persistence is incumbent-driven or a neutral equilibrium outcome.').

omega_variable(
    efficiency_loss_quantification,
    'What is the aggregate productivity and health cost borne by efficiency-seeking typists and data-entry workers forced to use QWERTY despite superior alternatives?',
    'Longitudinal studies of typist cohorts trained on Dvorak vs. QWERTY, controlling for hand size and physical predisposition; comparative studies of RSI incidence and severity; economic modeling of foregone productivity gains at the population level (billions of hours annually at 20–35% efficiency loss).',
    'High efficiency loss (>15% annual global typing productivity, >1M cases/year of preventable RSI) establishes the extraction component; low loss would reframe the constraint as minimal extraction despite lock-in (moving toward Rope classification from Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficiency_loss_quantification, empirical, 'Quantifying the welfare loss from QWERTY lock-in relative to available alternatives.').

omega_variable(
    reading_relationship_to_lapsed_alternatives,
    'Is this reading (incumbent_preservation) logically incompatible with the lapsed_alternatives reading, or do both readings describe different causal mechanisms that could coexist?',
    'Careful analysis of the causal claims in each reading. Incumbent_preservation claims: incumbents actively suppress alternatives. Lapsed_alternatives claims: alternatives fail to reach critical mass due to network dynamics, not suppression. These could be logically incompatible if incumbent suppression is the PRIMARY mechanism, and lapsed alternatives only explains what WOULD happen without suppression (counterfactual). Or they could coexist if both mechanisms operate simultaneously — incumbents defend, AND alternatives struggle with network effects independently.',
    'If foreclosing: only one reading can be true; the engine applies exclusion logic. If coexisting: both readings remain live, and the actual history likely exhibits both mechanisms in interaction. This affects how the readings are linked in network.affects_constraints (influence vs. forecloses vs. coexists_with).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_relationship_to_lapsed_alternatives, conceptual, 'Logical relationship between the incumbent_preservation and lapsed_alternatives readings of the QWERTY kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 1873, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1873, 0.0).
narrative_ontology:measurement_basis(qwer_tr_t1873, projected).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t1920, observed).
narrative_ontology:measurement(qwer_tr_t1940, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1940, 0.24).
narrative_ontology:measurement_basis(qwer_tr_t1940, observed).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement_basis(qwer_tr_t1960, observed).
narrative_ontology:measurement(qwer_tr_t1975, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1975, 0.4).
narrative_ontology:measurement_basis(qwer_tr_t1975, observed).
narrative_ontology:measurement(qwer_tr_t1985, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1985, 0.42).
narrative_ontology:measurement_basis(qwer_tr_t1985, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1873, 0.15).
narrative_ontology:measurement_basis(qwer_be_t1873, projected).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1920, 0.38).
narrative_ontology:measurement_basis(qwer_be_t1920, observed).
narrative_ontology:measurement(qwer_be_t1940, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1940, 0.52).
narrative_ontology:measurement_basis(qwer_be_t1940, observed).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1960, 0.61).
narrative_ontology:measurement_basis(qwer_be_t1960, observed).
narrative_ontology:measurement(qwer_be_t1975, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1975, 0.66).
narrative_ontology:measurement_basis(qwer_be_t1975, observed).
narrative_ontology:measurement(qwer_be_t1985, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement_basis(qwer_be_t1985, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1873, 0.18).
narrative_ontology:measurement_basis(qwer_su_t1873, projected).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1920, 0.35).
narrative_ontology:measurement_basis(qwer_su_t1920, observed).
narrative_ontology:measurement(qwer_su_t1940, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1940, 0.48).
narrative_ontology:measurement_basis(qwer_su_t1940, observed).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement_basis(qwer_su_t1960, observed).
narrative_ontology:measurement(qwer_su_t1975, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1975, 0.68).
narrative_ontology:measurement_basis(qwer_su_t1975, observed).
narrative_ontology:measurement(qwer_su_t1985, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1985, 0.71).
narrative_ontology:measurement_basis(qwer_su_t1985, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__incumbent_preservation_reading, 0.04).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence kernel decomposes into two structurally distinct constraint stories: incumbent_preservation_reading (this file) models QWERTY's persistence as the outcome of incumbent defense of sunk capital, yielding a Tangled Rope with identified extraction from efficiency-seekers and alternative-adopters. The lapsed_alternatives_reading models QWERTY's persistence as the outcome of neutral network-effect dynamics where alternatives fail to achieve critical-mass adoption despite technical superiority, yielding a Rope or Tangled Rope depending on whether alternative suppression is attributed to market dynamics or incumbent action. Both stories share the referent (QWERTY's observed 100+ year dominance) but contest the causal mechanism. The epsilon values differ: incumbent_preservation includes defensive suppression costs; lapsed_alternatives does not. Victim sets differ correspondingly. The readings coexist in historical and contemporary discourse — different analysts and stakeholders attribute QWERTY's persistence to one mechanism or the other — but the readings may also influence each other: if incumbent preservation is the dominant mechanism, it shapes the conditions under which alternatives lapse (by preventing critical-mass formation). The network edge represents structural dependency: incumbent_preservation influences lapsed_alternatives because incumbent suppression is part of what causes alternatives to fail at adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence__incumbent_preservation_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
