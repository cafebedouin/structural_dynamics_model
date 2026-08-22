% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__fitness_contingent_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Fitness-Contingent Personhood Boundary (State-Administered Standing Test)
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   The fitness-contingent reading of the personhood boundary conditions
 *   moral standing on demonstrated fitness, administered by state authority:
 *   entities that have not passed the assessment are pre-persons, owed
 *   nothing by the moral community. This story instantiates that single
 *   reading as a clean, epsilon-invariant constraint; its institutional
 *   history runs from Galton's eugenic doctrine (1883) through statutory
 *   compulsory sterilization (1907 onward), constitutional cover (Buck v.
 *   Bell, 1927), mass hereditary health courts (1933), and the terminal
 *   killing programs (1939-1945). The epsilon referent is fixed per the
 *   kernel-reading rule: the standing arrangement under contest is the
 *   fitness-contingent arrangement itself — the state-administered standing
 *   test and the exclusion it licenses — assessed as it actually operated,
 *   not as its modern attenuated defenders would have it operate. The
 *   claim/metric gap is deliberate and is the datum: the reading is CLAIMED
 *   as tangled_rope (it does coordinate a real problem — every moral
 *   community must draw the standing line somewhere — and the
 *   boundary-maintenance function is genuine, which is why sincere
 *   non-beneficiaries could hold it) while the authored metrics describe the
 *   arrangement's actual operation: near-maximal cost imposition on the
 *   classified-unfit, state coercion holding the boundary, and a substantial
 *   pseudo-scientific overlay on the assessment apparatus. base_properties
 *   describe the arrangement's characteristic operation in its mature phase
 *   (roughly 1927-1941); the measurement series runs the full lifecycle
 *   through the 1945 enforcement collapse.
 *
 * KEY AGENTS:
 *   - administering_state_authority: agenda-setter (institutional/arbitrage) — defines the fitness criterion, operates the assessment courts, collects classification authority and budget savings
 *   - fitness_certified_members: primary beneficiary (organized/constrained) — standing secured by the policed boundary, care resources conserved by exclusion
 *   - pre_fitness_excluded_infants: primary target (powerless/trapped) — assessed at or near birth; the classified are denied standing, care, and in the terminal instantiations life
 *   - classified_unfit_disabled_persons: primary target (powerless/trapped) — sterilized, confined, and in the terminal instantiations transferred to killing centers
 *   - objecting_parents_and_guardians: secondary payer and excluded voice (moderate/constrained) — bear direct loss with no seat in the assessment process
 *   - dissenting_medical_professionals: excluded voice (moderate/constrained) — internal professional objection absorbed or removed by the machinery
 *   - rival_reading_proponents: excluded voice (organized/mobile) — holders of the birth-threshold and potential-based criteria, overridden inside the jurisdiction
 *   - moral_philosophy_observers: analytical observer (analytical/analytical) — sees the full kernel contest and the structural delta this reading commits to
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.85).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.88).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Fitness-Contingent Personhood Boundary (State-Administered Standing Test)").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '5f0c10ea-89ac-4e13-a96d-9dbc40616606').
narrative_ontology:cs_kernel_codification('5f0c10ea-89ac-4e13-a96d-9dbc40616606', formalized).
narrative_ontology:cs_authority_grounding('5f0c10ea-89ac-4e13-a96d-9dbc40616606', extraction).
narrative_ontology:cs_interpretation_layer_present('5f0c10ea-89ac-4e13-a96d-9dbc40616606').
narrative_ontology:cs_reading_relation('5f0c10ea-89ac-4e13-a96d-9dbc40616606', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('5f0c10ea-89ac-4e13-a96d-9dbc40616606', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('5f0c10ea-89ac-4e13-a96d-9dbc40616606', foundational, standing_requires_demonstrated_fitness).
narrative_ontology:cs_axiom_status(standing_requires_demonstrated_fitness, holdable).
narrative_ontology:cs_axiom_grounding('5f0c10ea-89ac-4e13-a96d-9dbc40616606', standing_requires_demonstrated_fitness, empirically_contingent).
narrative_ontology:cs_axiom('5f0c10ea-89ac-4e13-a96d-9dbc40616606', secondary, community_may_exclude_pre_fitness_entities).
narrative_ontology:cs_axiom_status(community_may_exclude_pre_fitness_entities, holdable).
narrative_ontology:cs_axiom_grounding('5f0c10ea-89ac-4e13-a96d-9dbc40616606', community_may_exclude_pre_fitness_entities, instrumental).
narrative_ontology:cs_reference_frame('5f0c10ea-89ac-4e13-a96d-9dbc40616606', fitness_assessed_moral_community).
narrative_ontology:cs_drift_state('5f0c10ea-89ac-4e13-a96d-9dbc40616606', post_nuremberg_human_rights_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('5f0c10ea-89ac-4e13-a96d-9dbc40616606', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, fitness_certified_members).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, administering_state_authority).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_excluded_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, classified_unfit_disabled_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, objecting_parents_and_guardians).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, eugenic_fitness_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, state_classification_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures, health ministries, hereditary health courts, and eugenic boards define the fitness criterion, operate the assessment apparatus, and carry out its verdicts — compulsory sterilization, confinement, and in the terminal instantiations transfer to killing centers. The arrangement concentrates classification authority in this seat: it decides who counts, collects the budget savings from withheld care, and in the terminal instantiations appropriated the property of those it classified. Its exit is arbitrage — it wrote the criterion and can rewrite it; no external force fixes the standard.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, administering_state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% The assessed-fit population whose standing the arrangement secures. They pay taxes funding the apparatus but collect the larger flows: care resources conserved by exclusion, reproductive security from the sterilization of the classified-unfit, and the social position that comes from membership in a community with a policed boundary. Exit would mean renouncing the standing the arrangement confers; dissent was possible but carried social cost, and in the terminal instantiations risked the attention of the classification machinery itself.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, fitness_certified_members, beneficiary,
    organized, generational, constrained, national).

% Born into a jurisdiction that conditions standing on demonstrated fitness. Assessed at or shortly after birth against the state's criterion; those classified pre-fitness are denied the protections of the moral community — care withheld, and in the terminal instantiations registered, transferred, and killed under falsified records. The test is administered to them, never chosen by them; there is no exit from a classification performed on one's body before one can act.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_excluded_infants, payer,
    powerless, immediate, trapped, national).

% Children and adults classified unfit by hereditary health courts and eugenic boards — the disabled, the institutionalized, the 'hereditarily diseased.' Subjected to compulsory sterilization (over 400,000 in the German instantiation alone), confinement, labor extraction, and in the terminal instantiations transfer to killing centers. Family objection could be overridden by the courts; guardianship was transferred to the state precisely to remove the family's standing to refuse.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, classified_unfit_disabled_persons, payer,
    powerless, biographical, trapped, national).

% Parents and guardians of the classified-unfit. They bear the direct loss — children sterilized or removed — while holding no seat in the assessment process: consent was coerced, overridden by the courts, or in the terminal instantiations bypassed entirely by falsified death notices. Their objections were heard, when heard at all, only as petitions to the same courts that issued the verdicts.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, objecting_parents_and_guardians, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, objecting_parents_and_guardians, excluded).

% Physicians, nurses, and clerics who objected to classification-based exclusion. The assessment apparatus was staffed by their professions, and dissent cost positions, licenses, and in the terminal instantiations liberty — the machinery absorbed or removed the internal voices best placed to contest the criterion's validity.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, dissenting_medical_professionals, excluded,
    moderate, biographical, constrained, national).

% Holders of the sibling boundary criteria — religious communities insisting standing begins at birth, and philosophers grounding standing in potential for rational agency. They contest the kernel itself. The arrangement's enforcement overrode their criteria inside its jurisdiction (church opposition to sterilization statutes was legislatively defeated); their exit is mobility — the contest continued in jurisdictions and discourses the arrangement did not control.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, rival_reading_proponents, excluded,
    organized, generational, mobile, continental).

% Ethicists and historians assessing the reading from outside its framework — they see the full kernel contest, the family of readings, and the structural delta this reading commits to. They collect nothing and pay nothing; their seat is the record itself.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, moral_philosophy_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__fitness_contingent_reading, administering_state_authority).
narrative_ontology:fixing_cost_class(personhood_boundary__fitness_contingent_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines the boundary of the moral community by a single administrable criterion: the community coordinates its obligations of care, protection, and resource allocation around the fitness assessment's verdicts, and members share one rule for whom those obligations run to.
% TRANSFER_FUNCTION: Moves care resources, reproductive autonomy, legal protection, and — in the terminal instantiations — life itself from entities classified pre-fitness to the fitness-certified community and the administering state; it also moves classification authority itself to the state, which gains the power to decide standing case by case.
% ABSENT_VOICES: The classified-unfit are structurally voiceless — the criterion defines their testimony as counting for nothing. Objecting parents were heard only as petitioners to the courts that issued the verdicts; dissenting clinicians were absorbed or removed from their professions; proponents of the rival boundary criteria were legislatively overridden inside the jurisdiction and continued the contest only from outside it.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, care resources flow back to the classified-unfit, the state loses its classification authority over standing, reproductive autonomy returns to the classified and their families, and the certified community's obligations expand to cover the previously excluded — the moral community's boundary would be redrawn by whichever sibling criterion the community adopted, and every seat's position moves.
% FOUNDING_PROBLEM: Three problems the reading was built to solve: (1) the universal problem any moral community faces of bounding mutual obligation — who is owed care, protection, and resources; (2) the allocation problem of scarce care capacity in communities that could not sustain universal provision; (3) in its eugenic instantiation, the perceived problem of hereditary transmission of disability and dependency across generations. The reading offered one administrable answer to all three: condition standing on demonstrated fitness and let the state administer the test.
% FOUNDING_PROBLEM_CORROBORATION: The boundary-allocation half is corroborated outside the benefiting parties: proponents of the sibling readings concede the standing line must be drawn somewhere — the live kernel contest itself attests the problem is real. The hereditary-degeneration half is corroborated by no one outside the historical beneficiary set: post-war genetics dismantled the doctrine's empirical basis, and the Nuremberg doctors' trial record attests the degeneration framing functioned as authorization cover. No external source attests that the reading's full founding problem remains live.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__fitness_contingent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.85 because the arrangement's operative function is cost-shifting: it moves the entire cost of care, reproduction, and existence for the classified-unfit onto the excluded themselves while conserving resources for the certified community and classification authority for the state. Suppression is 0.88 — authored as the raw structural property it is, unscaled by power or scope — because the boundary does not hold by consent: it required statutes, health courts, coerced guardianship transfer, falsified records, and at the terminal end criminalization of dissent; rival boundary criteria were legislatively overridden inside the jurisdiction. Theater is 0.55: the assessment apparatus performed real classification with real consequences, but a large fraction of its activity was pseudo-scientific legitimation — craniometry, 'feeblemindedness' scales, heredity courts rubber-stamping predetermined verdicts — activity whose function was authorization, not measurement. Accessibility collapse is 0.65: inside the reading's framework the criterion is definitional and alternatives collapse almost completely, but the rival readings survived throughout in discourse and other jurisdictions, so alternatives never fully collapsed in fact. Resistance is 0.8: church opposition, parent petitions, dissenting clinicians, and finally the post-war human-rights repudiation make this among the most resisted constraints in its domain. The three measurement series share one time grid (1883-1945, eight points each). Suppression_requirement is authored because this story specifically tracks enforcement-capacity change: the ratchet from advocacy (0.25) through statutory machinery (0.48) to terminal secret-killing enforcement (0.92), then enforcement decay at regime defeat (0.35). Boltzmann note: identity_coordination is declared because the reading genuinely coordinates membership claims in the moral community against an evolving criterion — but this is exactly the cover-story risk class the FNL gaming warning names; the identity framing here does real coordinating AND carries the extraction, which is why the claim is tangled_rope rather than rope.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is legitimate administration: a criterion, a procedure, a docket. From the trapped victim seats the same structure is erasure: the test is administered to their bodies and its verdict removes them from the set of entities anything is owed to. From the certified seat it is security: standing confirmed by contrast with a policed outside. The excluded seats see the authorization structure the agenda-setter's seat renders invisible. Coalition note: the victim seats' powerlessness is manufactured, not natural — the arrangement deliberately destroyed coalition capacity by transferring guardianship to the state, isolating the classified in institutions, and falsifying records so families could not even aggregate their losses; any coalition-power consideration the engine applies to powerless agents should read this as designed isolation. The engine computes per-seat classifications from power, exit, and role; the authored tangled_rope claim does not adjudicate between the seats, and at the terminal instantiations the per-seat computation may well place victim-side seats in the pure-extraction range — that divergence is the measurement, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the administering state (d near the beneficiary end — the arrangement subsidizes it with classification authority, budget savings from withheld care, and in the terminal instantiations the appropriated property of the killed; its arbitrage exit means it bears no fixed cost of an arrangement it wrote and can rewrite) and the fitness-certified members (d near the beneficiary end — standing secured, resources conserved; exit constrained because renouncing the standing is costly and dissent carried social risk). Victims: pre-fitness infants and the classified-unfit (d near the full-target end — they bear the entire transfer: care withheld, reproduction terminated by compulsory sterilization, and in the terminal instantiations life itself; exit trapped because the classification is performed on them without consent and cannot be exited). The derivation from these beneficiary/victim declarations plus exit options places the victims at the target end without override, so no directionality_overrides entries are authored. The excluded seats (parents, dissenting clinicians, rival-reading proponents) carry no beneficiary/victim declaration and fall to canonical fallback — they are commentary-grade presence, not classification inputs.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two opposite mislabels. Reading the arrangement as pure extraction would erase its genuine coordination kernel: every moral community must draw the standing line somewhere, the reading does coordinate obligations around an administrable criterion, and the kernel problem it addresses is corroborated by the sibling readings' own existence — that kernel is why sincere non-beneficiaries could hold it. Reading it as pure coordination would erase the asymmetric extraction the historical record shows. The tangled_rope claim holds both, and at the terminal instantiations the operation approaches the pure-extraction boundary — the coordination story (community health) increasingly functioned as authorization cover while the operative function was elimination. Mandatrophy: the founding problem is contested — the boundary-allocation half is live (every community faces it), the hereditary-degeneration half is dead (discredited by post-war genetics) — while the disappearance verdict is world_rearranges. The contested-status × rearranges mismatch flags the zombie structure: a reading whose eugenic justification is dead but whose attenuated descendants persist in neonatal selection debates. The 1883-1945 series shows the full lifecycle: coordination story first, extraction ratcheting through enforcement buildup, theater rising as the underlying science failed, and collapse when enforcement capacity broke — fixing_cost is authored 'cheap' on exactly that evidence: the arrangement had no independent persistence and fell without structural barrier once enforcement failed (the formal halt of the terminal program in 1941 under public protest shows removal was a political decision, not a structural impossibility).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the personhood_boundary kernel — what changes structurally if a sibling reading (birth_threshold_reading or potential_based_reading) is adopted instead?',
    'The sibling stories instantiate the alternative conferring criteria; comparing victim sets, extraction structures, and enforcement requirements across the three-story family resolves what each reading commits to.',
    'Birth-threshold adoption eliminates the pre-fitness victim set entirely and collapses extraction toward zero; potential-based adoption narrows victims to severely disabled infants and removes routine state testing authority. The disagreement is located entirely in the conferring criterion: birth, demonstrated fitness, or potential.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: which reading of the personhood boundary kernel this constraint instantiates and what sibling adoption would structurally change.').

omega_variable(
    fitness_criterion_operationalization,
    'What counts as ''demonstrated fitness'' — who administers the assessment, against what standard, with what error tolerance at the margin?',
    'Examination of the historical assessment instruments and their validation record (hereditary health court files, eugenic board criteria) against modern psychometric and genetic knowledge.',
    'If the criterion is irreducibly examiner-relative, the classification power is arbitrary at the margin and the victim set is unstable — extraction widens beyond the authored value; if a validated criterion exists, the reading''s coordination claim strengthens and part of the measured extraction is re-priced as the cost of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_criterion_operationalization, empirical, 'Whether the fitness criterion is objectively administrable or examiner-relative.').

omega_variable(
    epsilon_moral_verdict_independence,
    'Does the reading''s own denial of moral standing to the excluded alter the extraction assessment, or is the arrangement''s cost-imposition structure independent of the reading''s moral verdict?',
    'Conceptual separation of structural cost imposition (who bears what under the standing arrangement) from moral valence (whether bearing it is a wrong); the framework''s epsilon measures the former over the fixed referent of the arrangement under contest.',
    'If extraction requires recognized moral patiency, the reading''s own lights drive epsilon toward zero and the arrangement reads as pure coordination; if cost imposition is structural, epsilon stays high regardless of the reading''s verdict — and the divergence between the reading''s claim and the structural data is itself the measurement the corpus exists to take.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_moral_verdict_independence, conceptual, 'Whether epsilon is indexed to the reading''s moral verdict or to structural cost imposition over the fixed referent.').

omega_variable(
    attenuated_persistence_structure,
    'Do post-1945 attenuated holders of the reading (selective-treatment and triage debates in neonatal bioethics) instantiate the same extraction structure as the historical eugenic instantiations, or a structurally different constraint?',
    'Compare decision authority (medical boards versus state courts), reversibility (treatment withdrawal versus compulsory sterilization and killing), and victim set across the historical and attenuated instantiations.',
    'If structurally continuous, the reading''s extraction profile persists in attenuated form and this story''s metrics understate the living arrangement; if discontinuous, the historical metrics describe a dead instantiation and the attenuated form requires its own story in the family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attenuated_persistence_structure, empirical, 'Whether attenuated modern instantiations share the historical extraction structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 1883, 1945).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1883, personhood_boundary__fitness_contingent_reading, theater_ratio, 1883, 0.28).
narrative_ontology:measurement_basis(pers_tr_t1883, observed).
narrative_ontology:measurement(pers_tr_t1895, personhood_boundary__fitness_contingent_reading, theater_ratio, 1895, 0.32).
narrative_ontology:measurement_basis(pers_tr_t1895, observed).
narrative_ontology:measurement(pers_tr_t1907, personhood_boundary__fitness_contingent_reading, theater_ratio, 1907, 0.38).
narrative_ontology:measurement_basis(pers_tr_t1907, observed).
narrative_ontology:measurement(pers_tr_t1920, personhood_boundary__fitness_contingent_reading, theater_ratio, 1920, 0.44).
narrative_ontology:measurement_basis(pers_tr_t1920, observed).
narrative_ontology:measurement(pers_tr_t1927, personhood_boundary__fitness_contingent_reading, theater_ratio, 1927, 0.48).
narrative_ontology:measurement_basis(pers_tr_t1927, observed).
narrative_ontology:measurement(pers_tr_t1933, personhood_boundary__fitness_contingent_reading, theater_ratio, 1933, 0.55).
narrative_ontology:measurement_basis(pers_tr_t1933, observed).
narrative_ontology:measurement(pers_tr_t1939, personhood_boundary__fitness_contingent_reading, theater_ratio, 1939, 0.62).
narrative_ontology:measurement_basis(pers_tr_t1939, observed).
narrative_ontology:measurement(pers_tr_t1945, personhood_boundary__fitness_contingent_reading, theater_ratio, 1945, 0.5).
narrative_ontology:measurement_basis(pers_tr_t1945, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t1883, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1883, 0.5).
narrative_ontology:measurement_basis(pers_be_t1883, observed).
narrative_ontology:measurement(pers_be_t1895, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1895, 0.56).
narrative_ontology:measurement_basis(pers_be_t1895, observed).
narrative_ontology:measurement(pers_be_t1907, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1907, 0.64).
narrative_ontology:measurement_basis(pers_be_t1907, observed).
narrative_ontology:measurement(pers_be_t1920, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1920, 0.7).
narrative_ontology:measurement_basis(pers_be_t1920, observed).
narrative_ontology:measurement(pers_be_t1927, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1927, 0.76).
narrative_ontology:measurement_basis(pers_be_t1927, observed).
narrative_ontology:measurement(pers_be_t1933, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1933, 0.83).
narrative_ontology:measurement_basis(pers_be_t1933, observed).
narrative_ontology:measurement(pers_be_t1939, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1939, 0.9).
narrative_ontology:measurement_basis(pers_be_t1939, observed).
narrative_ontology:measurement(pers_be_t1945, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1945, 0.68).
narrative_ontology:measurement_basis(pers_be_t1945, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1883, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1883, 0.25).
narrative_ontology:measurement_basis(pers_su_t1883, observed).
narrative_ontology:measurement(pers_su_t1895, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1895, 0.32).
narrative_ontology:measurement_basis(pers_su_t1895, observed).
narrative_ontology:measurement(pers_su_t1907, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1907, 0.48).
narrative_ontology:measurement_basis(pers_su_t1907, observed).
narrative_ontology:measurement(pers_su_t1920, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement_basis(pers_su_t1920, observed).
narrative_ontology:measurement(pers_su_t1927, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1927, 0.65).
narrative_ontology:measurement_basis(pers_su_t1927, observed).
narrative_ontology:measurement(pers_su_t1933, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1933, 0.8).
narrative_ontology:measurement_basis(pers_su_t1933, observed).
narrative_ontology:measurement(pers_su_t1939, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1939, 0.92).
narrative_ontology:measurement_basis(pers_su_t1939, observed).
narrative_ontology:measurement(pers_su_t1945, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement_basis(pers_su_t1945, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, potential_based_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three constraint stories under the epsilon-invariance principle: this file instantiates the fitness-contingent reading; birth_threshold_reading (standing unconditional at birth — victim set empty, epsilon near zero) and potential_based_reading (standing via potential — victim set narrowed to severely disabled infants, contested epsilon) are siblings. The readings differ in the conferring criterion, which fixes each one's victim set and extraction structure; no single story can carry all three criteria without making epsilon observer-dependent, which is why the label 'personhood boundary' is disambiguated here rather than complicated. The birth-threshold reading is the upstream established claim; this reading's historical collapse now exerts repudiation pressure on every non-birth criterion, and its record is the standard counterexample cited against both siblings. Family links run through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
