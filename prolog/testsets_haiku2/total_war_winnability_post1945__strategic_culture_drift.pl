% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War Winnability Constraint (Strategic Culture Drift Reading)
 *   domain: international_relations/strategic_culture
 *
 * SUMMARY:
 *   This constraint story instantiates the STRATEGIC_CULTURE_DRIFT reading of
 *   the kernel 'total_war_winnability_post1945'. The kernel is a contested
 *   claim about whether total war remains strategically viable after 1945.
 *   This reading asserts that total war capacity has atrophied not because it
 *   became structurally impossible (that is the
 *   structural_contraction_reading, grounded in nuclear deterrence), and not
 *   primarily because it became normatively illegitimate (that is the
 *   normative_reading_drop, grounded in international humanitarian law), but
 *   because strategic culture—the shared cognitive frameworks and
 *   professional norms within defense institutions—has forgotten how to think
 *   about, plan for, or articulate total war as a policy option. The capacity
 *   persists; the discourse does not. A piton: the machinery persists through
 *   institutional inertia, but the primary function (enabling strategic
 *   flexibility across the conflict spectrum) has atrophied, leaving only
 *   performative maintenance (academic debate about limited war's
 *   superiority).
 *
 * KEY AGENTS:
 *   - limited_war_strategic_community: Defense intellectuals and military strategists whose careers and institutional positions depend on the premise that total war is obsolete; they are the structural beneficiaries of the constraint.
 *   - military_education_establishment: War colleges and strategic studies centers that maintain the constraint through curricula, doctrine, and professional advancement criteria; the agenda-setter seat.
 *   - conflict_strategists_seeking_decisive_victory: Military planners and strategists who absorb the cost of operating under limited-war cognitive frames when they perceive conflicts as existential; the victim seat.
 *   - nuclear_armed_states: Maintain technical capacity for total war mobilization but are bound by strategic culture norms that prevent its articulation as a live policy option; the observer seat.
 *   - policy_innovation_advocates: Excluded from mainstream defense discourse; would argue for strategic reconsideration if admitted.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.68).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.72).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War Winnability Constraint (Strategic Culture Drift Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_culture").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'a50ee1ca-d6d3-439f-8f5e-6acef7889036').
narrative_ontology:cs_kernel_codification('a50ee1ca-d6d3-439f-8f5e-6acef7889036', implicit).
narrative_ontology:cs_authority_grounding('a50ee1ca-d6d3-439f-8f5e-6acef7889036', expertise).
narrative_ontology:cs_interpretation_layer_present('a50ee1ca-d6d3-439f-8f5e-6acef7889036').
narrative_ontology:cs_reading_relation('a50ee1ca-d6d3-439f-8f5e-6acef7889036', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('a50ee1ca-d6d3-439f-8f5e-6acef7889036', total_war_winnability_post1945__structural_contraction_reading, influences).
narrative_ontology:cs_axiom('a50ee1ca-d6d3-439f-8f5e-6acef7889036', foundational, strategic_culture_constitutive_of_possibility).
narrative_ontology:cs_axiom_status(strategic_culture_constitutive_of_possibility, holdable).
narrative_ontology:cs_axiom_grounding('a50ee1ca-d6d3-439f-8f5e-6acef7889036', strategic_culture_constitutive_of_possibility, instrumental).
narrative_ontology:cs_axiom('a50ee1ca-d6d3-439f-8f5e-6acef7889036', secondary, institutional_inertia_sustains_frameworks).
narrative_ontology:cs_axiom_status(institutional_inertia_sustains_frameworks, holdable).
narrative_ontology:cs_axiom_grounding('a50ee1ca-d6d3-439f-8f5e-6acef7889036', institutional_inertia_sustains_frameworks, empirically_contingent).
narrative_ontology:cs_reference_frame('a50ee1ca-d6d3-439f-8f5e-6acef7889036', limited_war_strategic_doctrine).
narrative_ontology:cs_drift_state('a50ee1ca-d6d3-439f-8f5e-6acef7889036', contemporary_crisis_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a50ee1ca-d6d3-439f-8f5e-6acef7889036', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_strategic_community).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, counterinsurgency_intellectual_apparatus).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, conflict_strategists_seeking_decisive_victory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defense intellectuals, military strategists, and academic theorists who built careers on the doctrine that total war is impractical, unwinnable, and obsolete in the post-1945 era. They maintain professional status and funding by sustaining the framework that treats limited war, counterinsurgency, and containment as the only viable strategic options. The constraint validates their intellectual project and their institutional positions within military education, policy advisory roles, and defense contracting.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_strategic_community, beneficiary,
    institutional, generational, identity_locked, global).

% Organizations, funding streams, and professional networks that have consolidated around counterinsurgency doctrine and limited war frameworks. Their legitimacy and resource flow depend on the premise that total war is not a live option—once total war is considered plausible again, the intellectual scaffolding and the budgetary priorities they rest on become vulnerable.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, counterinsurgency_intellectual_apparatus, beneficiary,
    institutional, generational, identity_locked, global).

% Military planners, strategists, and state actors who believe decisive victory is achievable and necessary in certain conflicts. They face a constraint on their strategic vocabulary and planning: proposing total war mobilization, unconditional surrender demands, or existential-stakes conflict framings is professionally and politically costly. They absorb the cost of operating within limited-war frames even when they perceive the conflict to have existential character.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, conflict_strategists_seeking_decisive_victory, payer,
    moderate, biographical, constrained, national).

% Possess the technical capacity for total war mobilization but operate under strategic culture norms that treat such mobilization as unthinkable. The constraint shapes their public posture and declared strategic planning, though their underlying capacity remains intact. Their observation role reflects their structural position: they maintain the capacity but the discourse constraint prevents its articulation as policy option.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, nuclear_armed_states, observer,
    institutional, civilizational, analytical, global).

% War colleges, strategic studies departments, and military doctrine centers that teach and enforce the post-1945 strategic consensus. They shape officer education around limited-war theory, counterinsurgency doctrine, and regional containment frameworks. The constraint is maintained through curricula, case study selection, speaker invitations, and the professional advancement criteria they enforce.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, military_education_establishment, agenda_setter,
    institutional, generational, mobile, global).

% Strategists, historical analysts, and operational planners who believe total war scenarios merit serious contingency analysis and strategic reconsideration but are excluded from mainstream defense policy discourse. Their exclusion is structural: proposing they be heard requires accepting that total war is a thinkable policy space, which the constraint's operation prevents.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, policy_innovation_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__strategic_culture_drift, limited_war_strategic_community).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__strategic_culture_drift, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains shared epistemic ground among defense strategists around the proposition that post-1945 total war is impractical and strategically obsolete, enabling coherent policy discourse and defense budgeting without constant re-contestation of foundational strategic premises.
% TRANSFER_FUNCTION: Transfers strategic credibility, funding, and institutional position away from those who would advocate existential-stakes total war scenarios toward those who articulate limited war, containment, and counterinsurgency frameworks. Moves professional authority from decisiveness-focused strategists to those invested in managed escalation and bounded conflict.
% ABSENT_VOICES: Military planners who believe total war remains a strategic option and should be studied; historians who argue total war capacity persists alongside cultural abandonment; strategists from non-Western traditions that do not internalize the post-1945 Western consensus; conflict theorists who question the inevitability of limited-war frameworks.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if total war returned to active elite discourse as a strategic possibility—policy would rearrange: defense budgeting would shift toward existential-stakes mobilization capacity; military education would teach total war scenarios alongside limited war; strategic planning would openly accommodate unconditional victory framings. But the disappearance verdict is contested because the constraint's actual function (maintaining discourse norms vs. reflecting genuine structural impossibility) is itself the kernel question.
% FOUNDING_PROBLEM: After 1945, total war mobilization appeared to have rendered itself obsolete through its own costs in WWII and the advent of nuclear weapons, generating a strategic-culture consensus that future great-power conflict must be contained within limited-war bounds to avoid existential catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Defense strategists from the limited-war school (Brodie, Schelling, Gray) attest the founding problem was solved by institutional embedding of graduated-response doctrine. Historians and strategists outside this institutional matrix (including non-Western strategic thinkers and historical revisionists) attest the founding problem persists: strategic capacity for total war has not been eliminated, only culturally suppressed, and that suppression may not hold under sufficient pressure.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, contested).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness value (0.68 endpoint) reflects that the constraint systematically moves credibility, institutional position, and funding toward the limited-war intellectual apparatus and away from those who would pursue total-war strategic options. The trajectory (rising from 0.45 to plateau at 0.68) models early institutionalization of limited-war doctrine (1945–1970s), consolidation within military education and policy circles (1980s–2000s), and equilibration at a stable high-extraction level (2000s–present) where the constraint is so deeply embedded it requires minimal active enforcement—it has become background assumption rather than actively defended claim. Theater_ratio rises sharply (0.35→0.71) because the constraint's function has atrophied: early decades legitimately coordinated strategic thinking around nuclear stability; later decades increasingly perform that legitimacy while the real strategic vocabulary narrowed. Suppression plateaus (0.48→0.72) because enforcement moved from active gatekeeping (suppressing total-war scholarship and strategic proposals) to passive normalization (total-war thinking simply does not appear in professional discourse anymore). Resistance remains moderate (0.58 endpoint) because conflict strategists and war historians maintain low-level contestation of the framework, but their voices are structurally excluded from policy influence. The piton diagnosis: the constraint persists not because anyone actively maintains it for its coordination function (which has dissolved) but because the beneficiary institutions that grew around it have political and financial incentive to keep it in place. If it vanished, policy would rearrange—but disappearance is contested because the constraint's actual causal power (restraining strategic thought vs. reflecting structural facts) is exactly what the kernel contest addresses.
 *
 * PERSPECTIVAL GAP:
 *   The limited-war strategic community perceives the constraint as a legitimate boundary condition: they believe total war genuinely is obsolete, and they see their intellectual work as capturing a hard truth about post-1945 strategy. From their seat, the constraint is natural law dressed up as strategic culture—it is true and well-justified. Conflict strategists and military planners perceive it as an extractive closure: they see strategic flexibility being removed by institutional fiat, and they observe that the capacity for total war still exists technically and that some conflicts may genuinely warrant existential-stakes framing. The gap is structural and deep. The engine computes per-seat classifications from directionality and power data; this narrative gap—the same constraint appearing natural-law-like from one seat and extractive-closure-like from another—is the operative definition of seat divergence that makes this a Piton rather than a unanimous classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Limited-war strategists have high institutional power (institutional atom), low vulnerability to the constraint (they benefit from it), and exit locked to the constraint (their professional identities, career paths, and funding depend on limited-war doctrine)—directionality near 0.1 (full beneficiary, identity-locked means they cannot exit, which amplifies benefit capture). Conflict strategists seeking total-war options have moderate power (moderate atom), high extraction (the constraint removes strategic options they believe valid), and constrained exit (they work within military structures that enforce limited-war frames)—directionality near 0.8 (substantial target, constrained means exit is costly). Military education establishment sets and enforces the constraint but does not extract from it in the way snares extract; their role is maintenance, not capture. They sit as agenda_setter: directionality around 0.4–0.5 (symmetric or slight benefit). This structural variation—high-benefit, high-cost, symmetric-maintenance sits—is what prevents a unanimous rock-solid classification and produces the seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as Piton: originally built to solve a real strategic-culture coordination problem (how to think clearly about limited war in the nuclear era), it has atrophied into a cultural norm that persists by institutional inertia. The founding problem (how to prevent escalation spirals into total war in a nuclear context) remains contested—the limited-war school says it was solved, others say it was never addressed, only culturally suppressed. The measurement series shows theater_ratio rising sharply and suppression plateauing while extractiveness stabilizes, the diagnostic piton signature: the constraint started with real coordination function (theater low), shifted to performing that function while the function atrophied (theater rises, suppression stabilizes), and now persists mostly through institutional performance with minimal real defensive work. No party actively maintains it against resistance—instead, the infrastructure that grew around it (military education, defense contracting, strategic studies departments) simply maintains the status quo because change would threaten their institutional positions. This is mandatrophy: the mandate (coordinating strategic thinking) has been abandoned, but the constraint persists due to institutional inertia and beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_vs_practice_ambiguity,
    'Does the constraint describe an actual erosion of total-war strategic capacity within military institutions, or merely an erosion of willingness to articulate it in professional discourse while capacity remains intact?',
    'War-game scenarios, strategic planning documents, military exercises, and officer education assessments showing whether total-war strategic thinking persists in institutional practice despite cultural silence about it.',
    'If capacity has genuinely atrophied (officers trained only in limited-war frameworks, logistical systems not built for total mobilization), the constraint is closer to a natural limit. If capacity persists in infrastructure and training, the constraint is purely discursive—a cognitive/cultural closure with no underlying structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_vs_practice_ambiguity, empirical, 'Whether strategic culture drift reflects genuine institutional forgetting or strategic silence about maintained capacity.').

omega_variable(
    kernel_contest_framing,
    'Is total-war winnability itself the right kernel question, or is the contest actually about whether strategic culture shapes strategic possibility versus structural facts constraining strategy?',
    'Philosophical analysis of how to assign causal weight to cultural norms vs. technical constraints vs. normative frameworks in determining what strategies are ''possible''. No empirical resolution; this is a conceptual/methodological question about how to decompose causality.',
    'If cultural norms are constitutive of strategic possibility (pragmatist epistemology), then this reading''s piton diagnosis makes sense: the constraint is real because culture is causally operative. If structural facts determine possibility and culture is mere interpretation, then this reading mis-locates the causal force and the structural_contraction reading is more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'Whether strategic culture determines strategic possibility or merely interprets possibilities determined by structure.').

omega_variable(
    beneficiary_legitimacy_fusion,
    'Are the limited-war strategists genuine beneficiaries of a constraint that serves extraction, or do they legitimately represent a superior strategic framework that happens to align with institutional interests?',
    'Historical and comparative analysis: do non-Western strategic traditions that did not adopt limited-war frameworks report different strategic outcomes? Do conflict outcomes in adherents vs. non-adherents of limited-war doctrine differ systematically? Can the strategic theory be separated from the institutional position that benefits from it?',
    'If limited-war doctrine is genuinely superior, the beneficiary relationship is incidental alignment, not extraction. If doctrine is defensive pseudo-theory rationalizing institutional positions, the constraint is pure extraction with ideological cover. The truth likely sits between.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_legitimacy_fusion, empirical, 'Whether limited-war strategic superiority explains doctrine adoption or institutional interests explain doctrine persistence.').

omega_variable(
    suppression_mechanism_structure,
    'Is the suppression of total-war strategic thinking structural (economic and career incentives that make it costly to propose), internalized (defense intellectuals have genuinely absorbed limited-war norms as natural), or both?',
    'Ethnographic study of defense institutions and strategic communities, examining whether suppression persists after career incentives are removed; exit interviews with strategists who left the field.',
    'If structural, fixing the constraint requires removing incentive structures. If internalized, the constraint persists even if incentives shift—professional identities have fused with limited-war frameworks. If both, fixing requires both incentive restructuring and cognitive reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structure, empirical, 'Suppression in strategic-culture constraints: structural economic incentives vs. internalized professional identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twsc_tr_t0, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(twsc_tr_t0, observed).
narrative_ontology:measurement(twsc_tr_t10, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(twsc_tr_t10, observed).
narrative_ontology:measurement(twsc_tr_t20, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(twsc_tr_t20, observed).
narrative_ontology:measurement(twsc_tr_t30, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(twsc_tr_t30, observed).
narrative_ontology:measurement(twsc_tr_t40, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 40, 0.64).
narrative_ontology:measurement_basis(twsc_tr_t40, observed).
narrative_ontology:measurement(twsc_tr_t50, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 50, 0.68).
narrative_ontology:measurement_basis(twsc_tr_t50, observed).
narrative_ontology:measurement(twsc_tr_t60, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 60, 0.7).
narrative_ontology:measurement_basis(twsc_tr_t60, observed).
narrative_ontology:measurement(twsc_tr_t70, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 70, 0.71).
narrative_ontology:measurement_basis(twsc_tr_t70, observed).
narrative_ontology:measurement(twsc_tr_t80, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 80, 0.71).
narrative_ontology:measurement_basis(twsc_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(twsc_be_t0, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(twsc_be_t0, observed).
narrative_ontology:measurement(twsc_be_t10, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(twsc_be_t10, observed).
narrative_ontology:measurement(twsc_be_t20, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(twsc_be_t20, observed).
narrative_ontology:measurement(twsc_be_t30, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(twsc_be_t30, observed).
narrative_ontology:measurement(twsc_be_t40, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(twsc_be_t40, observed).
narrative_ontology:measurement(twsc_be_t50, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 50, 0.67).
narrative_ontology:measurement_basis(twsc_be_t50, observed).
narrative_ontology:measurement(twsc_be_t60, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(twsc_be_t60, observed).
narrative_ontology:measurement(twsc_be_t70, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 70, 0.68).
narrative_ontology:measurement_basis(twsc_be_t70, observed).
narrative_ontology:measurement(twsc_be_t80, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(twsc_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(twsc_su_t0, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(twsc_su_t0, observed).
narrative_ontology:measurement(twsc_su_t10, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(twsc_su_t10, observed).
narrative_ontology:measurement(twsc_su_t20, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(twsc_su_t20, observed).
narrative_ontology:measurement(twsc_su_t30, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(twsc_su_t30, observed).
narrative_ontology:measurement(twsc_su_t40, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 40, 0.69).
narrative_ontology:measurement_basis(twsc_su_t40, observed).
narrative_ontology:measurement(twsc_su_t50, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 50, 0.7).
narrative_ontology:measurement_basis(twsc_su_t50, observed).
narrative_ontology:measurement(twsc_su_t60, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(twsc_su_t60, observed).
narrative_ontology:measurement(twsc_su_t70, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 70, 0.72).
narrative_ontology:measurement_basis(twsc_su_t70, observed).
narrative_ontology:measurement(twsc_su_t80, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 80, 0.72).
narrative_ontology:measurement_basis(twsc_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__strategic_culture_drift, 0.25).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__structural_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'total_war_winnability_post1945'. The kernel is decomposed into three structurally distinct constraint stories, each modeling a different mechanism by which total war is excluded from strategic options after 1945: (1) STRATEGIC_CULTURE_DRIFT (this story)—institutional forgetting via professional norms in defense education. (2) NORMATIVE_READING_DROP—normative illegitimacy via international humanitarian law and treaty commitments (Article 2(4) UN Charter). (3) STRUCTURAL_CONTRACTION_READING—nuclear deterrence made total war structurally impossible regardless of cultural norms. Each reading has different ε values (none, some, all of the winnability constraint reflects cultural atrophy vs. normative prohibition vs. structural fact), different beneficiary/victim structures, and different policy implications. They are linked here as a constraint family: all three are live positions in scholarly debate; none forecloses the others within the strategic studies community. The three readings influence each other: if one mechanism is shown to dominate, it constrains what the others can claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__strategic_culture_drift, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
