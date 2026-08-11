% ============================================================================
% CONSTRAINT STORY: alpha_m_supercriticality_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alpha_m_supercriticality_kernel_flat_control, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: alpha_m_supercriticality_kernel_flat_control
 *   human_readable: Dirac Quantization Condition (e·g = 2πn) as Fixed Relation Governing Magnetic Monopole Coupling
 *   domain: theoretical_physics/cosmology/speculative_astrophysics
 *
 * SUMMARY:
 *   This story authors the Dirac quantization condition (e·g = 2πn) as a
 *   flat, undecomposed constraint: a single mathematical relation that all
 *   parties — monopole theorists, unification advocates, supercriticality
 *   researchers, cosmologists, and skeptics — accept as their fixed starting
 *   point even while disputing its physical significance, its explanatory
 *   necessity, and the legitimacy of the speculative programs (supercritical
 *   alpha_m, cosmic monopole abundance) erected on top of it. The relation
 *   itself is a genuine mathematical theorem given standard premises
 *   (single-valuedness of the wavefunction, U(1) bundle topology) and is
 *   claimed here as a mountain. The contestation the source material
 *   describes — parties who 'argue about' the condition without disputing it
 *   as a starting point — lands not in the mathematics (which is uncontested)
 *   but in the surrounding research economy: which downstream physical claims
 *   (monopole existence, supercriticality phenomenology, unification
 *   necessity) the mathematical relation is taken to license, and how much
 *   institutional weight should ride on a topological result whose motivating
 *   case (explaining charge quantization) may already be independently
 *   explained by anomaly cancellation in the Standard Model.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alpha_m_supercriticality_kernel_flat_control, 0.08).
domain_priors:suppression_score(alpha_m_supercriticality_kernel_flat_control, 0.04).
domain_priors:theater_ratio(alpha_m_supercriticality_kernel_flat_control, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alpha_m_supercriticality_kernel_flat_control, extractiveness, 0.08).
narrative_ontology:constraint_metric(alpha_m_supercriticality_kernel_flat_control, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(alpha_m_supercriticality_kernel_flat_control, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(alpha_m_supercriticality_kernel_flat_control, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(alpha_m_supercriticality_kernel_flat_control, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alpha_m_supercriticality_kernel_flat_control, mountain).
narrative_ontology:human_readable(alpha_m_supercriticality_kernel_flat_control, "Dirac Quantization Condition (e·g = 2πn) as Fixed Relation Governing Magnetic Monopole Coupling").
narrative_ontology:topic_domain(alpha_m_supercriticality_kernel_flat_control, "theoretical_physics/cosmology/speculative_astrophysics").

domain_priors:emerges_naturally(alpha_m_supercriticality_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(alpha_m_supercriticality_kernel_flat_control, alpha_m_supercriticality_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alpha_m_supercriticality_kernel_flat_control, magnetic_monopole_theorists).
narrative_ontology:constraint_beneficiary(alpha_m_supercriticality_kernel_flat_control, grand_unification_program_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(alpha_m_supercriticality_kernel_flat_control, alpha_m_supercriticality_researchers).
narrative_ontology:constraint_victim(alpha_m_supercriticality_kernel_flat_control, cosmologists_studying_monopole_abundance).
narrative_ontology:constraint_vindicates(alpha_m_supercriticality_kernel_flat_control, charge_quantization_from_topology).
narrative_ontology:constraint_vindicates(alpha_m_supercriticality_kernel_flat_control, dirac_string_gauge_invariance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build entire research programs — grand unified theories, cosmic string models, monopole search experiments — on the premise that the quantization condition guarantees a discrete, calculable coupling structure. Their careers, grant proposals, and theoretical frameworks derive legitimacy from the condition's mathematical necessity; if it were merely a convention rather than a forced consequence of gauge invariance, much of the motivation for monopole searches would weaken, though the relation itself would be unaffected by that motivational dependence.
narrative_ontology:constraint_stakeholder(alpha_m_supercriticality_kernel_flat_control, magnetic_monopole_theorists, beneficiary,
    organized, civilizational, analytical, universal).

% Cite the condition as evidence that fine structure constant values are not arbitrary but constrained by a deeper topological structure, supporting funding cases for unification-focused theory and experiment. They did not create the relation and cannot alter it, but its existence is repeatedly invoked to justify continued institutional investment in searches for magnetic charge.
narrative_ontology:constraint_stakeholder(alpha_m_supercriticality_kernel_flat_control, grand_unification_program_advocates, beneficiary,
    institutional, generational, constrained, universal).

% Work on the speculative regime where alpha_m (derived from alpha_e via the Dirac relation) becomes supercritical — strong-coupling behavior for a hypothetical monopole sector that would produce vacuum instabilities or pair-production analogues to the Schwinger effect for electric charge. They must accept the quantization relation as fixed and then argue, against skeptics, that its consequences at strong coupling are physically meaningful rather than an artifact of extrapolating perturbative electrodynamics past its domain of validity. Their exit option is limited: reject the relation and the entire supercriticality question dissolves, but the relation itself is not something they can renegotiate.
narrative_ontology:constraint_stakeholder(alpha_m_supercriticality_kernel_flat_control, alpha_m_supercriticality_researchers, payer,
    moderate, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(alpha_m_supercriticality_kernel_flat_control, alpha_m_supercriticality_researchers, excluded).

% Point out that the supercritical alpha_m regime implied by e·g = 2πn (with alpha_e ≈ 1/137) sits so far outside any regime where perturbative field theory or semiclassical monopole solutions are trustworthy that claims about 'supercriticality' may be extrapolation dressed as prediction. They are rarely the ones invited to referee grand-unification funding proposals built on the same relation, and their skepticism about the physical content of the supercritical claims is largely confined to journal commentary rather than the funding conversation.
narrative_ontology:constraint_stakeholder(alpha_m_supercriticality_kernel_flat_control, perturbative_qed_skeptics, excluded,
    organized, biographical, mobile, global).

% Must reconcile the Dirac-quantized coupling with the cosmological monopole problem (why we do not observe the predicted relic abundance of magnetic monopoles). They inherit the quantization relation as an unmovable premise and then absorb the interpretive cost of explaining away its cosmological consequences via inflationary dilution — extra theoretical machinery whose necessity traces back to accepting the fixed relation in the first place.
narrative_ontology:constraint_stakeholder(alpha_m_supercriticality_kernel_flat_control, cosmologists_studying_monopole_abundance, payer,
    moderate, civilizational, constrained, universal).

% Evaluate the mathematical necessity of the Dirac condition (derivable rigorously from single-valuedness of the quantum wavefunction around a Dirac string, or from the topology of U(1) bundles) as distinct from the physical claims layered on top of it (that monopoles exist, that supercriticality has observable consequences, that unification requires this structure). They can affirm the mathematics while remaining agnostic or skeptical about every downstream physical program that invokes it.
narrative_ontology:constraint_stakeholder(alpha_m_supercriticality_kernel_flat_control, independent_theoretical_physicists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this is not a human coordination problem but a mathematical consistency requirement: any consistent quantum theory containing both electric and magnetic charges must quantize their product, on pain of the wavefunction failing to be single-valued around a Dirac string. It 'coordinates' theoretical consistency, not competing human interests.
% TRANSFER_FUNCTION: No resources, work, or status literally transfer through the relation itself. What moves through the SURROUNDING research economy is funding attention and career legitimacy: institutions and theorists who frame research programs around consequences of the fixed relation (unification, monopole searches, supercriticality) draw grant support and publication space that flows away from skeptics who treat the extrapolated consequences as unverified.
% ABSENT_VOICES: Experimentalists working on null monopole searches (who have found nothing after decades) are rarely centered in theoretical discussions building further speculative structure atop the quantization relation; their negative results are acknowledged but do not visibly slow the theoretical program's momentum.
% DISAPPEARANCE_RATIONALE: The mathematical relation itself cannot disappear — it is a theorem given the premises (topological charge quantization, gauge invariance, single-valuedness). What COULD disappear or weaken is the surrounding research program's confidence in the relation's PHYSICAL relevance (if magnetic monopoles are shown not to exist in any accessible regime, or if alternative formulations displace the topological argument). If that confidence collapsed, funding and career structures built on monopole/unification research would rearrange substantially; the mathematics would remain true but interpretively idle, which is why parties dispute whether 'the constraint disappearing' means anything coherent.
% FOUNDING_PROBLEM: Dirac (1931) sought to explain why electric charge is quantized at all — introducing a hypothetical magnetic monopole and requiring the electromagnetic vector potential's Dirac string singularity to be physically unobservable, which forces e·g = 2πn. The founding problem was charge quantization, not monopole existence per se; the monopole was originally a tool to explain an independently observed fact (quantized electric charge) via topology.
% FOUNDING_PROBLEM_CORROBORATION: Mathematical physicists outside the monopole-search community (differential geometers, gauge theory specialists) corroborate that the quantization argument is a genuine, freestanding topological result independent of whether monopoles are ever found — this part is essentially undisputed. Whether the FOUNDING problem (explaining observed electric charge quantization) is still 'live' as a motivation, versus having been superseded by other explanations (e.g. anomaly cancellation in the Standard Model, which quantizes charge without requiring monopoles), is disputed between monopole-program advocates (who say the topological explanation remains the deepest available) and skeptics (who say the Standard Model already explains charge quantization without any monopole assumption, making the monopole-based derivation explanatorily redundant for its original purpose).
narrative_ontology:disappearance_verdict(alpha_m_supercriticality_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(alpha_m_supercriticality_kernel_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(alpha_m_supercriticality_kernel_flat_control, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-11',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(alpha_m_supercriticality_kernel_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(alpha_m_supercriticality_kernel_flat_control, 0.08, 'claude-sonnet-5', 'dirac_magnetic_matter_2026_20260811_143746', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alpha_m_supercriticality_kernel_flat_control_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(alpha_m_supercriticality_kernel_flat_control, ExtMetricName, E),
    domain_priors:suppression_score(alpha_m_supercriticality_kernel_flat_control, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(alpha_m_supercriticality_kernel_flat_control),
    narrative_ontology:constraint_metric(alpha_m_supercriticality_kernel_flat_control, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(alpha_m_supercriticality_kernel_flat_control, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(alpha_m_supercriticality_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) because the relation itself commands no resources and confers no direct rents — nobody profits from e·g = 2πn being true in the way a toll collector profits from a bridge. The low but nonzero and slowly rising extractiveness reflects the diffuse funding-attention effect: as more speculative literature (supercriticality, monopole cosmology) treats the relation as a springboard, institutional attention concentrates modestly around programs that invoke it, at some opportunity cost to skeptical or alternative research directions. Suppression is very low (0.04) — nobody is coerced into accepting the mathematics, and the relation would hold whether or not any physicist ever discussed it. Accessibility collapse is high (0.88): once the topological derivation is understood, there is no coherent alternative formulation of a U(1) gauge theory with both electric and magnetic charges that avoids the condition — this is what makes it Mountain-like rather than conventional. Resistance is low (0.15): what resistance exists is aimed not at the mathematics but at inflated physical claims built on it (supercriticality skeptics, anomaly-cancellation advocates who see the monopole story as unnecessary), which the theater_ratio (rising modestly to 0.12) tracks as an increasing share of speculative literature invoking the relation as apparent justification for programs that do not strictly require it.
 *
 * PERSPECTIVAL GAP:
 *   Independent theoretical physicists (analytical seat) see a clean mathematical theorem with essentially zero extraction and near-total accessibility collapse — the purest mountain reading. Monopole theorists and unification advocates (organized/institutional beneficiary seats) see the same relation as licensing an entire research program, and experience its truth as vindicating and career-sustaining. Supercriticality researchers and cosmologists (moderate/payer seats) experience the relation as a fixed premise they must accept and then defend against skeptics when extending it into speculative strong-coupling or cosmological territory — for them the relation is not extractive itself, but accepting it commits them to absorbing the interpretive costs of its downstream puzzles (the monopole problem, supercriticality's uncertain physical content). Skeptics (organized/mobile seat) are structurally excluded from the funding conversation despite holding a defensible position (anomaly cancellation already explains charge quantization without monopoles), which is why the story frames founding_problem_status as contested rather than resolved in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (monopole theorists, unification advocates) are coded near the beneficiary end because the relation's mathematical necessity is repeatedly cited to legitimize their research investment, even though they neither created nor control the relation — the benefit is reputational and funding-adjacent, not a direct capture of resources from a victim. There is no true victim group in this story (no base_properties.victims declared) because no party bears a directly imposed cost from the mathematics being true; the closest analogue — skeptics and null-result experimentalists losing attention share — is captured instead through the excluded role and the absent_voices answer, which is the correct channel for a diffuse opportunity-cost dynamic rather than a declared victim group. This asymmetry (beneficiaries present, victims absent) is exactly the FSM (false-summit) configuration the schema anticipates, which is why omegas below document the natural-law-vs-motivated-emphasis ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (explaining quantized electric charge) is contested as live vs dead: the Standard Model's anomaly-cancellation mechanism arguably already solves it without monopoles, which would make the Dirac-monopole explanation for THAT specific purpose obsolete — but the topological quantization result survives as mathematically true regardless, and continues to be invoked for other purposes (unification aesthetics, monopole search justification). This is precisely why the disappearance_verdict is 'contested' rather than 'world_unchanged': the mathematics is permanent, but the institutional program built citing it as motivation is not, and conflating the two would mislabel a mathematically necessary constraint as mere extractive scaffolding, or conversely mislabel a genuinely motivated-but-optional research emphasis as an unchallengeable natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mathematical_necessity_vs_program_motivation,
    'Is the Dirac quantization condition purely a mathematical consequence of assumed premises (topological charge quantization, gauge invariance) that would hold with zero institutional stake either way, or does its continued centrality in physics discourse partly reflect the motivated interests of a research community (monopole theorists, unification advocates) whose funding and career narratives benefit from foregrounding it?',
    'Track citation and funding patterns for monopole/unification research against independent measures of the relation''s necessity for explaining observed phenomena (e.g., whether anomaly-cancellation-only explanations of charge quantization, which need no monopole, receive proportionate attention and funding).',
    'If the relation''s centrality is driven substantially by motivated program-building rather than by the mathematics standing on its own explanatory merits, this constraint would show the false-summit-mountain signature: a genuine mathematical mountain with declared beneficiaries whose interests inflate its perceived physical necessity beyond what the mathematics alone would warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mathematical_necessity_vs_program_motivation, conceptual, 'Whether the relation''s prominence reflects pure mathematical necessity or partly reflects the interests of the community that benefits from its centrality.').

omega_variable(
    supercriticality_physical_content,
    'Does the notion of ''supercritical alpha_m'' derived from the Dirac relation via alpha_e ≈ 1/137 describe a physically meaningful strong-coupling regime for a hypothetical monopole sector, or is it an artifact of extrapolating semiclassical/perturbative reasoning far outside its domain of validity, given that no consistent strong-coupling monopole field theory has been established?',
    'Non-perturbative lattice or duality-based (e.g. Montonen-Olive / S-duality) calculations of monopole dynamics at the coupling strength implied by Dirac quantization, compared against the semiclassical supercriticality claims currently in circulation.',
    'If supercriticality claims are shown to be extrapolation artifacts, the entire alpha_m_supercriticality_researchers program''s cost-bearing position weakens considerably, and the diffuse extraction this story attributes to speculative literature invoking the relation would be overstated; if the claims survive non-perturbative scrutiny, the researchers'' position and the underlying extraction estimate are corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supercriticality_physical_content, empirical, 'Whether supercritical alpha_m claims have genuine non-perturbative physical content or are perturbative extrapolation artifacts.').

omega_variable(
    founding_problem_redundancy,
    'Given that Standard Model anomaly cancellation independently explains observed electric charge quantization without invoking monopoles at all, is the Dirac monopole-based derivation now explanatorily redundant for its ORIGINAL founding purpose, even though it remains true and continues to motivate independent research (unification, monopole cosmology)?',
    'Comparative philosophy-of-science analysis of whether anomaly cancellation and monopole-based quantization are genuinely competing explanations of the same phenomenon or complementary results addressing different levels of structure (gauge consistency vs. topological necessity).',
    'If genuinely redundant, the founding_problem_status of ''dead'' (for the original 1931 motivation) would be more strongly supported, while the mathematical relation itself remains valid and load-bearing for other, later-arising physical questions — clarifying that mandatrophy analysis must track PURPOSES, not just the persistence of the mathematical fact itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_redundancy, conceptual, 'Whether the Standard Model''s anomaly-cancellation explanation of charge quantization renders the monopole-based derivation redundant for its founding purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alpha_m_supercriticality_kernel_flat_control, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alph_tr_t0, alpha_m_supercriticality_kernel_flat_control, theater_ratio, 0, 0.05).
narrative_ontology:measurement(alph_tr_t15, alpha_m_supercriticality_kernel_flat_control, theater_ratio, 15, 0.07).
narrative_ontology:measurement(alph_tr_t30, alpha_m_supercriticality_kernel_flat_control, theater_ratio, 30, 0.09).
narrative_ontology:measurement(alph_tr_t45, alpha_m_supercriticality_kernel_flat_control, theater_ratio, 45, 0.1).
narrative_ontology:measurement(alph_tr_t60, alpha_m_supercriticality_kernel_flat_control, theater_ratio, 60, 0.11).
narrative_ontology:measurement(alph_tr_t75, alpha_m_supercriticality_kernel_flat_control, theater_ratio, 75, 0.12).
narrative_ontology:measurement(alph_tr_t90, alpha_m_supercriticality_kernel_flat_control, theater_ratio, 90, 0.12).

% Extraction over time
narrative_ontology:measurement(alph_be_t0, alpha_m_supercriticality_kernel_flat_control, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(alph_be_t15, alpha_m_supercriticality_kernel_flat_control, base_extractiveness, 15, 0.06).
narrative_ontology:measurement(alph_be_t30, alpha_m_supercriticality_kernel_flat_control, base_extractiveness, 30, 0.07).
narrative_ontology:measurement(alph_be_t45, alpha_m_supercriticality_kernel_flat_control, base_extractiveness, 45, 0.08).
narrative_ontology:measurement(alph_be_t60, alpha_m_supercriticality_kernel_flat_control, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(alph_be_t75, alpha_m_supercriticality_kernel_flat_control, base_extractiveness, 75, 0.09).
narrative_ontology:measurement(alph_be_t90, alpha_m_supercriticality_kernel_flat_control, base_extractiveness, 90, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(alpha_m_supercriticality_kernel_flat_control, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alpha_m_supercriticality_kernel_flat_control, information_standard).
narrative_ontology:boltzmann_floor_override(alpha_m_supercriticality_kernel_flat_control, 0.02).

% DUAL FORMULATION NOTE:
% This story is authored FLAT per the construction-perturbation control instruction: the Dirac quantization condition is treated as one constraint, not decomposed into an originalist-mathematics reading versus a physical-necessity reading versus a supercriticality-program reading, even though the source material and the perspectival_gap commentary above show these readings pulling apart internally. No sibling reading files exist for this control condition; the internal tension is captured through stakeholder seat divergence and omegas rather than through network links to sibling constraint files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
