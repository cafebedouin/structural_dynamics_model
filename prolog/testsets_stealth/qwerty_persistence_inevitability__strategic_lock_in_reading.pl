% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: QWERTY Persistence as Manufacturer-Engineered Lock-In (Strategic Lock-In Reading)
 *   domain: technology_history/political_economy
 *
 * SUMMARY:
 *   Under this reading, the persistence of the QWERTY letter arrangement is
 *   not an accident that nobody chose to undo but an arrangement that was
 *   built and maintained: the leading manufacturers combined in 1893 to ship
 *   one layout on every machine, wired the training schools into the standard
 *   through partnership programs, gated which rival arrangements could reach
 *   production through dealer exclusivity and cross-licensing control, and
 *   thereby converted a design choice into the only employable typing skill.
 *   The coordination function is real (a shared layout genuinely solved
 *   labor-market matching, parts compatibility, and training-economy
 *   problems), and the transfer running through the same structure is also
 *   real (typists bore the ergonomic load and the retraining barriers; the
 *   combining firms collected the rents of uniformity). This file is the
 *   strategic-lock-in member of the QWERTY-persistence constraint family; its
 *   sibling, the path-dependency reading, is linked via
 *   network.affects_constraints and carries a different epsilon, a different
 *   beneficiary structure, and its own classification.
 *
 * KEY AGENTS:
 *   - remington_leading_manufacturers: agenda-setting beneficiary (institutional/arbitrage) — convened the 1893 standardization combination, controlled which arrangements reached production, collected the licensing and uniformity rents
 *   - commercial_typing_schools: coordinating beneficiary with payer exposure (organized/constrained) — one curriculum and interchangeable graduates, with the classroom side committed to the designated layout
 *   - professional_typists: primary target (powerless/trapped) — bore the training captivity, the ergonomic load, and the retraining barriers; absent from the deliberations that fixed their instrument
 *   - repetitive_strain_injury_patients: target (moderate/constrained) — carried the cumulative physical costs and entered the record decades after the arrangement was fixed
 *   - alternative_layout_inventors: excluded challenger (moderate/trapped) — held frequency-optimized designs but had no route to production through the combination's gatekeeping
 *   - keyboard_standards_bodies: inheriting administrator (institutional/constrained) — maintains defaults, certifications, and curricula; bound by the installed base it administers
 *   - equipment_procurement_offices: institutional payer with genuine coordination gains (powerful/constrained) — buys and trains at scale; migration cost exceeds any single budget horizon
 *   - economic_historians: analytical observer (analytical/analytical) — the external check on both the efficiency justification and the design narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.66).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.42).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence as Manufacturer-Engineered Lock-In (Strategic Lock-In Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, 'aca9629f-1113-4d67-b982-2c72765c3118').
narrative_ontology:cs_kernel_codification('aca9629f-1113-4d67-b982-2c72765c3118', distributed).
narrative_ontology:cs_authority_grounding('aca9629f-1113-4d67-b982-2c72765c3118', distributed).
narrative_ontology:cs_reading_relation('aca9629f-1113-4d67-b982-2c72765c3118', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('aca9629f-1113-4d67-b982-2c72765c3118', foundational, standardization_control_constituted_deliberate_rent_extraction).
narrative_ontology:cs_axiom_status(standardization_control_constituted_deliberate_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('aca9629f-1113-4d67-b982-2c72765c3118', standardization_control_constituted_deliberate_rent_extraction, empirically_contingent).
narrative_ontology:cs_axiom('aca9629f-1113-4d67-b982-2c72765c3118', secondary, typist_costs_externalized_by_design).
narrative_ontology:cs_axiom_status(typist_costs_externalized_by_design, holdable).
narrative_ontology:cs_axiom_grounding('aca9629f-1113-4d67-b982-2c72765c3118', typist_costs_externalized_by_design, empirically_contingent).
narrative_ontology:cs_reference_frame('aca9629f-1113-4d67-b982-2c72765c3118', cartel_engineered_standard_regime).
narrative_ontology:cs_drift_state('aca9629f-1113-4d67-b982-2c72765c3118', post_antitrust_network_effect_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aca9629f-1113-4d67-b982-2c72765c3118', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_leading_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, equipment_procurement_offices).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, repetitive_strain_injury_patients).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_schools).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, equipment_procurement_offices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominated typewriter manufacture from the 1870s onward and convened the 1893 combination under which the leading makers agreed to ship a single letter arrangement on every machine. Set the terms of cross-licensing, ran exclusive dealer and training-school partnership programs, and decided which layout proposals reached production. Collected licensing income and the margin advantages of uniform parts and shared repair networks across the combined firms; bore essentially none of the typing-side costs of the chosen arrangement. Exit never mattered to them: they owned the patents, the factories, and the distribution channels, and could move capital to whatever arrangement they designated next.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_leading_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Taught touch typing as a mass service from the 1880s onward. A single agreed letter arrangement meant one curriculum, one set of drills, and graduates whose skills transferred to any employer's machines, making the schools' product interchangeable labor. In exchange they committed their classrooms, textbooks, and certification examinations to whichever arrangement the manufacturers designated, and passed the physical strain of that arrangement on to their students' subsequent working lives. Leaving the partnership would have meant graduating students into a fragmented machine market.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_schools, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_schools, payer).

% The office workforce that learned the designated arrangement through months of training and then typed on it for decades. Their skill was certified, salaried, and portable only within the standard; retraining to a different arrangement mid-career meant months of lost speed and income with no employer willing to sponsor it. They had no seat in the 1893 deliberations that fixed their working instrument and no individual recourse against the fatigue and repetition injuries the finger-load patterns produced. Individually replaceable and largely non-unionized, they absorbed the arrangement's physical costs as a condition of employment.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists, payer,
    powerless, biographical, trapped, global).

% Typists and office workers whose hands, wrists, and shoulders carried the cumulative load of the arrangement's uneven finger distribution, with the left hand and upper rows handling disproportionate traffic. Diagnosed and treated decades after the arrangement was fixed, they paid for their own medical care and workplace accommodations, and their injury data entered the layout debate only in the late twentieth century, long after the training pipeline had made switching economically irrational for their employers.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, repetitive_strain_injury_patients, payer,
    moderate, biographical, constrained, global).

% Patent holders and researchers, most prominently August Dvorak and William Dealey in the 1930s, who designed arrangements around measured letter frequencies and finger alternation and sought adoption through schools, government trials, and licensing. They stood outside the manufacturer combination that controlled which arrangement reached production, watched trials get discontinued and textbooks remain bound to the incumbent layout, and could reach the market only through the goodwill of the firms and institutions their designs threatened.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors, excluded,
    moderate, biographical, trapped, global).

% Twentieth- and twenty-first-century standards committees, operating-system vendors, and procurement authorities that inherited the arrangement and now decide, release by release, which layout ships as the default on new hardware and software. They maintain the character mappings, certification tests, and educational benchmarks that keep the arrangement operative. Changing the default would break compatibility with the entire installed base of documents, skills, and muscle memory, so each body reaffirms the inherited choice while administering it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_standards_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Government agencies and corporations that buy input devices by the thousand and train staff on the standard curriculum. A shared arrangement lets them hire certified operators from any school and move staff across departments and offices without retraining; it also binds every device they purchase, every form template they run, and every hire they onboard to the same inherited layout, so the cost of migrating even one workflow to an alternative exceeds any single office's budget horizon.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, equipment_procurement_offices, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, equipment_procurement_offices, beneficiary).

% The scholarly community examining how the arrangement came to persist: business historians reading trust minutes and firm correspondence, economists modeling network effects and switching costs, archivists publishing the trial records. They collect nothing from the arrangement and bear none of its costs; their publications are the principal external check on both the manufacturers' efficiency justifications and the design-based narrative's evidentiary claims.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, economic_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_leading_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizing one letter arrangement across all typewriters solved a real multi-party problem: schools could train once, employers could hire interchangeable certified operators, manufacturers could share parts catalogs and repair networks, and agencies could place temporary stenographers anywhere without retraining.
% TRANSFER_FUNCTION: Moves ergonomic burden, retraining costs, and switching costs from manufacturers and standard-setters to typists and their employers; moved standardization-control rents, licensing income, and uniformity margins from the typing public to the combining manufacturers; moves the cost of any alternative layout onto the alternative's proponents.
% ABSENT_VOICES: The typists whose hands would carry the arrangement and the inventors of rival arrangements were absent from the 1893 standardization deliberations, which were negotiated among manufacturers; typists encountered the chosen layout only after it was fixed, and ergonomic researchers entered the record half a century later. Their objections exist in the historical record as injury data and discontinued trial reports rather than as seats at the table.
% DISAPPEARANCE_RATIONALE: If the arrangement lost its enforced status overnight, the keyboard ecosystem would fragment and then reconverge at enormous cost: curricula rewritten, hundreds of millions of users retrained or left with degraded speed, hardware relabeled, procurement specifications redrafted, and the labor market for typing skills temporarily broken while a successor standard emerged. Every named seat's situation depends on the arrangement holding.
% FOUNDING_PROBLEM: The combination was built to end destructive competition among typewriter manufacturers: divergent layouts fragmented the training pipeline, parts inventories, and dealer networks, and price wars threatened the combined firms' margins. Fixing one letter arrangement across all member machines stabilized the product line, the labor supply, and the price structure simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Business-history and antitrust records corroborate from outside the beneficiary set: the combination was dissolved under antitrust action in the 1910s, its price-and-layout stabilization purpose lapsed with it, and no successor cartel reconstituted the founding bargain, yet the arrangement persisted through the electronic era. Economic historians affiliated with neither the manufacturers nor the layout-reform movement document the dissolution and the persistence independently; the rival scholarly camp disputes the design narrative itself but does not dispute that the founding cartel bargain is gone.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.66 because the arrangement's costs (uneven finger loading, retraining barriers, suppressed alternatives) were concentrated on parties with no seat in its design, while its benefits accrued substantially to the combining firms; the referent is the standing QWERTY arrangement itself as this reading assesses it, not any endorsed replacement. Suppression is authored at 0.42 as a raw structural property, unscaled by power or scope: the coercive machinery (contractual exclusivity, training lock-in, trial discontinuation) has decayed from its cartel-era peak, leaving coordination-cost barriers rather than prohibition, which is why the temporal suppression series falls while extractiveness rises as the locked population grew. Theater ratio 0.32 reflects a maintenance apparatus that is partly functional (standards certification, curriculum upkeep) and partly ceremonial (efficiency justifications repeated without re-examination). Accessibility collapse 0.68: individual opt-out remains possible, but once the installed-base economics are visible, collective alternatives collapse to near-inaccessibility. Resistance 0.45 records the recurring but defeated reform movements. Claimed type is tangled_rope on structural grounds independent of these metrics: genuine coordination function, asymmetric transfer, and enforcement that remains active in institutional form (defaults, certification, procurement) even after the cartel's contractual machinery dissolved. All three tracked metrics run on one shared seven-point grid spanning 1873-2023 so the engine samples a complete row at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the manufacturer seat the arrangement is a product-design achievement that stabilized an industry and created a labor market; from the typist seat it is an instrument fixed by others, learned under duress of employability, and physically costly; from the standards-body seat it is an inheritance whose revision cost always exceeds the revision budget; from the excluded inventor seat it is a closed door. The engine derives these divergent classifications from the declared positions and exits; this story authors the positions and declines to adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: remington_leading_manufacturers sit near the full-beneficiary end (they collected the rents and faced no exit pressure); professional_typists, repetitive_strain_injury_patients, and alternative_layout_inventors sit near the full-target end (trapped or constrained exits amplify their effective extraction). Two overrides correct derivations the structural data alone would get wrong. Commercial_typing_schools derive a strongly beneficiary-side d from their beneficiary declaration, but their curriculum captivity makes them genuine dual-positioned agents, so organized-power agents are overridden to d=0.40. Equipment_procurement_offices derive a strongly target-side d from their primary payer role, but their labor-interchangeability gains are real and concentrated, so powerful-power agents are overridden to d=0.55, near symmetric. No override is applied at the institutional power atom because the two institutional agents (manufacturers, standards bodies) legitimately occupy opposite ends and must be distinguished by their declared roles, not flattened by a shared override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stabilizing price, layout, and labor-supply competition among combining manufacturers) died with the trust's dissolution, yet the arrangement persists and still rearranges the world if removed, which is precisely the dead-problem-plus-world-rearranges mismatch the genealogy interview exists to flag. The tangled_rope classification prevents the two symmetric mislabelings: a pure-coordination reading would erase the engineered asymmetry (the victims were selected by the standard-setters, not by the physics of typing), while a pure-extraction reading would erase the genuine coordination function (a shared layout really did solve training and labor-matching problems that no alternative solved at the time). The piton risk is real and monitored: post-cartel, the gain flow thins toward diffuse, and if the omega on contemporary enforcement resolves toward inertia, the computed type should migrate toward piton. Within this reading, the historical capture record keeps the constraint tangled_rope rather than piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the strategic_lock_in_reading of the qwerty_persistence_inevitability kernel: was QWERTY''s persistence produced by deliberate manufacturer coordination (trust standardization, training partnerships, dealer exclusivity), or by accident-driven path dependency with no strategic beneficiaries, as the sibling path_dependency_reading holds?',
    'Archival adjudication: published trust minutes, Remington and successor-firm correspondence, training-partnership contracts, and dated adoption timelines that distinguish pre-combination diffusion from post-1893 entrenchment.',
    'If the sibling reading is correct, the beneficiary and victim sets declared here dissolve (no cartel rents collected, no engineered targets), epsilon drops toward negligible, and the constraint reclassifies from tangled_rope toward rope or piton. The disagreement is located specifically in the causal status of the 1893 standardization agreement and the training-partnership program.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, empirical, 'Kernel contest between strategic lock-in and accident-driven path dependency.').

omega_variable(
    ergonomic_cost_magnitude,
    'How large are the ergonomic and retraining costs borne by typists under the QWERTY arrangement, relative to the coordination benefits the shared layout provides?',
    'Longitudinal musculoskeletal epidemiology across layouts, controlled retraining-time studies, and labor-market analyses of switching costs for certified typists.',
    'Sets the victim side of the ledger: substantial measured costs support the tangled_rope reading at high epsilon; trivially small costs collapse the victim declarations and push the reading toward benign coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergonomic_cost_magnitude, empirical, 'Magnitude of typist-borne ergonomic and retraining costs.').

omega_variable(
    dvorak_evidence_quality,
    'Was the Dvorak arrangement actually superior, given the documented methodological flaws in the 1930s-40s trials cited in its favor?',
    'Modern controlled studies of layout performance and fatigue with pre-registered protocols, independent of the original naval and civil-service trial records.',
    'If no superior alternative was suppressed, the enforcement machinery defended a standard rather than blocked a better one, lowering measured suppression and shifting interpretive weight from engineered harm toward ordinary standardization politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_evidence_quality, empirical, 'Whether a genuinely superior alternative was suppressed.').

omega_variable(
    contemporary_enforcement_status,
    'Is present-day maintenance of the arrangement (operating-system defaults, education curricula, procurement specifications, standards certification) active enforcement requiring coordinated will, or inertial self-perpetuation that would persist without any administrator?',
    'Counterfactual analysis of default-change episodes (national keyboard-standard revisions, vendor experiments with alternative defaults) and observation of whether deviations revert without enforcement action.',
    'If maintenance is merely inertial, the constraint has degraded from actively enforced coordination-plus-transfer toward administered inertia, moving the computed type toward piton despite the historical cartel record.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contemporary_enforcement_status, conceptual, 'Whether the arrangement still requires active enforcement or runs on inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1873, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_strategic_lockin_tr_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1873, 0.1).
narrative_ontology:measurement(qwerty_strategic_lockin_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.22).
narrative_ontology:measurement(qwerty_strategic_lockin_tr_t1913, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1913, 0.26).
narrative_ontology:measurement(qwerty_strategic_lockin_tr_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1936, 0.41).
narrative_ontology:measurement(qwerty_strategic_lockin_tr_t1972, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1972, 0.34).
narrative_ontology:measurement(qwerty_strategic_lockin_tr_t1998, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1998, 0.37).
narrative_ontology:measurement(qwerty_strategic_lockin_tr_t2023, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2023, 0.32).

% Extraction over time
narrative_ontology:measurement(qwerty_strategic_lockin_be_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1873, 0.28).
narrative_ontology:measurement(qwerty_strategic_lockin_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.54).
narrative_ontology:measurement(qwerty_strategic_lockin_be_t1913, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1913, 0.62).
narrative_ontology:measurement(qwerty_strategic_lockin_be_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1936, 0.59).
narrative_ontology:measurement(qwerty_strategic_lockin_be_t1972, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1972, 0.56).
narrative_ontology:measurement(qwerty_strategic_lockin_be_t1998, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1998, 0.64).
narrative_ontology:measurement(qwerty_strategic_lockin_be_t2023, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2023, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_strategic_lockin_su_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1873, 0.35).
narrative_ontology:measurement(qwerty_strategic_lockin_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.7).
narrative_ontology:measurement(qwerty_strategic_lockin_su_t1913, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1913, 0.74).
narrative_ontology:measurement(qwerty_strategic_lockin_su_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1936, 0.63).
narrative_ontology:measurement(qwerty_strategic_lockin_su_t1972, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1972, 0.52).
narrative_ontology:measurement(qwerty_strategic_lockin_su_t1998, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1998, 0.44).
narrative_ontology:measurement(qwerty_strategic_lockin_su_t2023, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2023, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, resource_allocation).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'QWERTY persistence is inevitable' conflates two structurally distinct claims and is decomposed per the epsilon-invariance principle into a two-story constraint family. This file authors the strategic_lock_in_reading: persistence as manufacturer-engineered lock-in, with cartel beneficiaries, engineered victims, and tangled_rope structure at epsilon 0.66. The sibling file authors the path_dependency_reading: persistence as accident-driven path dependency with no strategic beneficiaries, empty beneficiary set, and correspondingly low epsilon. The upstream/downstream link runs from this reading to the sibling because the design-based account, if corroborated, supplies the mechanism the path-dependency account treats as exogenous; each story carries its own stable epsilon and its own stakeholder surface, and neither hedges across the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_inevitability__strategic_lock_in_reading, organized, 0.4).
constraint_indexing:directionality_override(qwerty_persistence_inevitability__strategic_lock_in_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
