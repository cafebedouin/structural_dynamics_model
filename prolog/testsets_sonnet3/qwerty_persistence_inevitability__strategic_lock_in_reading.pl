% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Keyboard Standardization as Manufacturer-Engineered Lock-In
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the strategic lock-in reading of the QWERTY
 *   persistence kernel: rather than treating the layout's dominance as an
 *   emergent accident of early adoption (the path_dependency_reading, a
 *   separate constraint), this reading treats the 1893 typewriter
 *   manufacturer combination as a deliberate act of cartel standardization
 *   whose persistence was actively engineered through control of touch-typing
 *   training and certification infrastructure. Under this reading, the
 *   coordination function (a single layout enables training at scale) is real
 *   but is layered with asymmetric extraction: the cartel members and their
 *   training-school partners captured rents from the standardization they
 *   controlled, while working typists bore retraining lock-in and ergonomic
 *   costs with no institutional channel to validate superior alternatives.
 *   The mechanical jam-avoidance justification for the original letter
 *   placement became irrelevant once electric typewriters and then electronic
 *   keyboards eliminated jamming risk, but the trained-workforce and
 *   certification lock-in persisted regardless — the founding problem died
 *   while the arrangement did not.
 *
 * KEY AGENTS:
 *   - typewriter_trust_1893_cartel_members: primary beneficiary and agenda-setter (organized/arbitrage) — coordinated the standard and captured training-market rents
 *   - working_typists: primary target (powerless/trapped) — bore retraining lock-in and non-transferable skill investment
 *   - repetitive_strain_injury_sufferers: secondary target (powerless/trapped) — bore ergonomic costs of a design whose original justification expired
 *   - alternative_layout_inventors: excluded voice (powerless/trapped) — had no institutional channel to validate superior layouts once training infrastructure was captured
 *   - labor_historians: analytical observer (analytical) — assesses contested documentary record of the cartel's coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.61).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Keyboard Standardization as Manufacturer-Engineered Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '4b3fad82-8fb4-46b0-964d-04f4f8a66053').
narrative_ontology:cs_kernel_codification('4b3fad82-8fb4-46b0-964d-04f4f8a66053', distributed).
narrative_ontology:cs_authority_grounding('4b3fad82-8fb4-46b0-964d-04f4f8a66053', extraction).
narrative_ontology:cs_interpretation_layer_present('4b3fad82-8fb4-46b0-964d-04f4f8a66053').
narrative_ontology:cs_reading_relation('4b3fad82-8fb4-46b0-964d-04f4f8a66053', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('4b3fad82-8fb4-46b0-964d-04f4f8a66053', foundational, standardization_was_coordinated_rent_extraction).
narrative_ontology:cs_axiom_status(standardization_was_coordinated_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('4b3fad82-8fb4-46b0-964d-04f4f8a66053', standardization_was_coordinated_rent_extraction, empirically_contingent).
narrative_ontology:cs_axiom('4b3fad82-8fb4-46b0-964d-04f4f8a66053', secondary, training_certification_capture_sustains_lockin_independent_of_technical_merit).
narrative_ontology:cs_axiom_status(training_certification_capture_sustains_lockin_independent_of_technical_merit, holdable).
narrative_ontology:cs_axiom_grounding('4b3fad82-8fb4-46b0-964d-04f4f8a66053', training_certification_capture_sustains_lockin_independent_of_technical_merit, empirically_contingent).
narrative_ontology:cs_reference_frame('4b3fad82-8fb4-46b0-964d-04f4f8a66053', mechanical_jam_avoidance_necessity).
narrative_ontology:cs_drift_state('4b3fad82-8fb4-46b0-964d-04f4f8a66053', post_electronic_keyboard_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('4b3fad82-8fb4-46b0-964d-04f4f8a66053', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_trust_1893_cartel_members).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, touch_typing_school_operators).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_manufacturers_post_standardization).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, working_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, stenographers_and_clerical_workers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, repetitive_strain_injury_sufferers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The 1893 combination of major typewriter manufacturers (Remington, Yost, Densmore, Caligraph, and others) agreed to standardize on the QWERTY layout as part of a broader patent-pooling and market-division arrangement. Having fixed the layout, the trust members funded and controlled touch-typing curricula that trained a generation of typists exclusively on QWERTY, converting a manufacturing decision into a labor-market fact. They collect from every subsequent machine sale and from the network effect their own coordination created.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_trust_1893_cartel_members, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_trust_1893_cartel_members, beneficiary).

% Businesses and institutes that built curricula, textbooks, and certification systems around QWERTY touch-typing, often financially entangled with or licensed by the manufacturers. They profit from selling training keyed specifically to the standardized layout and have no incentive to teach or validate any competing arrangement.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, touch_typing_school_operators, beneficiary,
    moderate, biographical, constrained, national).

% Later manufacturers, including computer keyboard makers, inherited a standardized layout with a captive trained workforce and captive installed hardware base. They face essentially zero cost from the standardization and substantial cost avoidance from never having to justify or re-engineer around ergonomics, since the market already expects QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_manufacturers_post_standardization, beneficiary,
    institutional, civilizational, arbitrage, global).

% Clerical workers, largely women entering the typing profession from the 1880s onward, who were trained exclusively on the trust-standardized layout as a condition of employability. Their labor-market value became keyed to QWERTY proficiency specifically, making any individual switch to a superior layout a career-ending act of un-training themselves out of a marketable skill.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, working_typists, payer,
    powerless, biographical, trapped, national).

% Office workers whose employability depended on certified typing speed measured on QWERTY machines. Certification systems built around the standardized layout meant their accumulated skill was non-transferable to any alternative arrangement, regardless of that arrangement's merits.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, stenographers_and_clerical_workers, payer,
    powerless, biographical, trapped, national).

% Typists and later computer users who developed strain injuries plausibly attributable to QWERTY's non-ergonomic finger-load distribution and letter placement, a design consequence of the original mechanical jam-avoidance rationale that outlived its mechanical justification once typewriters (and later keyboards) no longer jammed. They bear a physical cost with no practical exit given labor-market lock-in.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, repetitive_strain_injury_sufferers, payer,
    powerless, biographical, trapped, national).

% Designers of alternative layouts (most prominently Dvorak) who demonstrated measurable speed and ergonomic advantages but could never overcome the trained workforce and certification infrastructure the cartel had already built. Their evidence was structurally excluded from the market test because the test itself (typing certification, employer hiring criteria) was owned by the incumbents.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors, excluded,
    powerless, biographical, trapped, national).

% Researchers who examine trust records, patent-pool agreements, and typing-school curricula to assess whether QWERTY's persistence reflects coordinated manufacturer strategy or emergent path dependency. Their analysis is contested precisely because the surviving documentary record of the 1893 arrangements is incomplete.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, labor_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_trust_1893_cartel_members).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The 1893 standardization did solve a genuine coordination problem: without a single agreed layout, typewriter buyers, typists, and training schools each faced incompatible machines and non-transferable skills. A single layout let training scale and let the labor market for typists function at all.
% TRANSFER_FUNCTION: The arrangement moves rents from every subsequent typist and keyboard user to the manufacturers who fixed the standard and to the training institutions who built certification around it — specifically via captured retraining costs, foreclosed competition from superior layouts, and unaddressed ergonomic injury costs externalized onto workers.
% ABSENT_VOICES: Alternative layout inventors and the typists who would have benefited from switching are structurally absent from the standard-setting process — the training and certification infrastructure that would validate a superior alternative was owned by the same cartel that benefited from the incumbent layout, so dissent had no institutional channel.
% DISAPPEARANCE_RATIONALE: If the standardization enforcement machinery (training curricula, certification systems, employer hiring norms keyed to QWERTY) vanished overnight, the demonstrated efficiency gains of alternative layouts (contested but real in trials) would have room to compete on merits, and a portion of accumulated RSI costs and retraining barriers would dissolve; the manufacturer cartel's captured advantage would disappear with it.
% FOUNDING_PROBLEM: Early typewriter markets in the 1870s-1880s had incompatible key layouts across manufacturers, fragmenting the training and labor market for typists and slowing adoption of the technology overall.
% FOUNDING_PROBLEM_CORROBORATION: Labor historians and ergonomics researchers outside the manufacturing and training-school beneficiary set attest that the original mechanical jam-avoidance and market-fragmentation problems QWERTY solved no longer exist in any technology since electric and electronic keyboards eliminated jamming risk decades ago; the standardization persists via trained-workforce lock-in and certification infrastructure rather than the problem it was built to solve.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.68 by the end of the interval because, under this reading, the persistence mechanism is not merely technological momentum but active rent extraction via controlled training and certification systems that manufacturers financially benefited from maintaining. Suppression is authored at 0.61, reflecting the structural barrier (non-transferable certified skill, employer hiring norms keyed to the standard) rather than raw coercion — no one is physically prevented from learning Dvorak, but the labor market makes doing so career-costly. Theater ratio rises from 0.10 to 0.42 over the interval as the original mechanical jam-avoidance rationale became obsolete (electric and electronic keyboards do not jam) while the standardization apparatus (certification, training curricula, 'this is simply how typing is taught') persisted as increasingly performative justification for what is now pure lock-in. Accessibility collapse is high (0.72) because once the trained workforce and certification infrastructure exist, individual defection to an alternative layout is essentially unavailable regardless of the alternative's merits. Resistance is authored moderate-low (0.35) because Dvorak advocacy and ergonomic-injury litigation exist but have never mounted a labor-market-scale challenge to the certification apparatus.
 *
 * PERSPECTIVAL GAP:
 *   From the cartel/manufacturer seat, standardization looks like pure coordination success: a common layout let the market for typing labor and machines scale efficiently. From the working-typist seat, the same structure operates as an inherited cost with no negotiated consent — they entered a labor market where the terms (QWERTY proficiency) were already fixed by actors they never bargained with, and switching costs make any individual exit irrational even where collective exit might be beneficial.
 *
 * DIRECTIONALITY LOGIC:
 *   The 1893 cartel members and downstream keyboard manufacturers sit at the beneficiary end: they set the standard, controlled the training apparatus built around it, and collect from the network effect their own coordination produced, with arbitrage-grade exit (they can switch products or markets without cost). Working typists, stenographers, and RSI sufferers sit at the target end: trapped exit options, because their accumulated occupational skill is keyed specifically to the standardized layout and un-training is not a viable option within a working career. Alternative layout inventors are excluded rather than merely victimized — the standard-setting and validation infrastructure that would test their claims was owned by the same actors who benefited from the incumbent standard, foreclosing the market test itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict mismatch is diagnostic here: the founding problem (incompatible layouts fragmenting an emergent labor market) is dead — no technology since electronic keyboards has any jamming-avoidance rationale for QWERTY's letter placement — yet the disappearance_verdict is world_rearranges, meaning real arrangements (trained workforces, certification systems, employer hiring norms) still depend on the standard's persistence. This is exactly the capture/zombie signature R5 is designed to surface: a constraint whose stated justification expired decades before its enforcement machinery did. Classifying this as tangled_rope rather than snare preserves that the original 1893 standardization did solve a genuine coordination problem (fragmented layouts genuinely impeded training-market formation); classifying it as tangled_rope rather than mountain or rope refuses to let the coordination function launder the ongoing extraction that training-school and certification lock-in impose on typists who have no real exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cartel_intent_vs_emergent_lockin,
    'Does the surviving documentary record of the 1893 typewriter manufacturer combination establish deliberate strategic intent to extract rents via training-market control, or does it show only an ordinary standardization agreement whose later lock-in effects were unintended?',
    'Archival analysis of trust correspondence, patent-pool agreements, and typing-school funding records from 1893-1910; comparison against the counterfactual path_dependency_reading''s claim that no such coordinated intent is documented.',
    'If intent is well-corroborated, this reading''s tangled_rope classification with concentrated cartel beneficiaries stands as the historically accurate account. If the record shows only incidental standardization without coordinated training-market capture, the path_dependency_reading better fits the evidence and this reading''s beneficiary claims would need substantial revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_intent_vs_emergent_lockin, empirical, 'Whether 1893 cartel documentary evidence supports strategic intent versus emergent path dependency.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the strategic_lock_in_reading and path_dependency_reading readings of QWERTY persistence disagree — is it about the initial 1893 standardization event, or about the mechanism of subsequent persistence (training/certification lock-in vs. pure network-effect momentum)?',
    'Decompose the causal chain: (1) initial layout selection, (2) 1893 cross-manufacturer standardization, (3) touch-typing curriculum formation, (4) multi-decade persistence. Assess whether the two readings actually agree on stages 1 and 4 while disagreeing sharply on stages 2-3.',
    'If the readings agree on most of the causal chain and disagree only on whether stage 2-3 involved coordinated rent extraction versus emergent institutional isomorphism, the practical classification difference (tangled_rope vs weaker rope/mountain-adjacent reading) hinges on a comparatively narrow historical question about cartel coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating precisely where the two kernel readings diverge in the causal chain of QWERTY''s persistence.').

omega_variable(
    ergonomic_injury_causal_attribution,
    'How much of documented repetitive strain injury among typists and keyboard users is causally attributable specifically to QWERTY''s letter placement versus general repetitive-motion risk present in any keyboard layout including untested alternatives?',
    'Comparative ergonomic studies of finger-load distribution and injury incidence across QWERTY, Dvorak, and Colemak users controlling for typing volume and posture.',
    'If QWERTY-specific ergonomic harm is well-established, the victim-cost component of this reading''s extractiveness score is strongly supported. If injury rates are comparable across layouts, that component of the extraction claim weakens and the primary extraction becomes purely the retraining/certification lock-in rather than ongoing physical harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ergonomic_injury_causal_attribution, empirical, 'Whether RSI harm is specifically attributable to QWERTY design versus general keyboard use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1873, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1873, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t1873, observed).
narrative_ontology:measurement(qwer_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.15).
narrative_ontology:measurement_basis(qwer_tr_t1893, observed).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1930, 0.28).
narrative_ontology:measurement_basis(qwer_tr_t1930, observed).
narrative_ontology:measurement(qwer_tr_t1975, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1975, 0.38).
narrative_ontology:measurement_basis(qwer_tr_t1975, observed).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement_basis(qwer_tr_t2000, observed).
narrative_ontology:measurement(qwer_tr_t2020, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(qwer_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1873, 0.2).
narrative_ontology:measurement_basis(qwer_be_t1873, observed).
narrative_ontology:measurement(qwer_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.45).
narrative_ontology:measurement_basis(qwer_be_t1893, observed).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement_basis(qwer_be_t1930, observed).
narrative_ontology:measurement(qwer_be_t1975, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1975, 0.63).
narrative_ontology:measurement_basis(qwer_be_t1975, observed).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement_basis(qwer_be_t2000, observed).
narrative_ontology:measurement(qwer_be_t2020, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(qwer_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1873, 0.15).
narrative_ontology:measurement_basis(qwer_su_t1873, observed).
narrative_ontology:measurement(qwer_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.5).
narrative_ontology:measurement_basis(qwer_su_t1893, observed).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement_basis(qwer_su_t1930, observed).
narrative_ontology:measurement(qwer_su_t1975, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement_basis(qwer_su_t1975, observed).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement_basis(qwer_su_t2000, observed).
narrative_ontology:measurement(qwer_su_t2020, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2020, 0.61).
narrative_ontology:measurement_basis(qwer_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.1).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% This story and qwerty_persistence_inevitability__path_dependency_reading are sibling readings of the same kernel (qwerty_persistence_inevitability). Both describe the same historical standardization event and its persistence, but this reading authors a substantially higher extractiveness (0.68 vs. a much lower value expected under path dependency), names concentrated 1893-cartel beneficiaries where the sibling names none, and classifies as tangled_rope where the sibling classifies closer to a coordination-dominant type. Per the ε-invariance principle, these are two constraints, not one constraint measured two ways — each carries its own stable ε and its own stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
