% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Democratic Participation Speech Protection Hierarchy
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the democratic-participation reading of the
 *   speech protection kernel: the constitutional commitment that speech
 *   protection is strongest for political expression necessary for
 *   self-governance. Unlike the absolutist reading (near-categorical
 *   protection for all speech) or the dignity reading (protection conditional
 *   on avoiding structural subordination), the democratic-participation
 *   reading establishes a tiered regime where political speech receives
 *   maximally strong protection and non-political expression (commercial,
 *   artistic, intimate) receives intermediate protection subject to
 *   regulation. The core legitimacy claim is that democracy requires robust
 *   political discourse and that tiered protection serves this foundational
 *   need. The constraint is claimed as rope (solving the collective-action
 *   problem of political discourse) while measurements capture the extraction
 *   vector: the tiering transfers protection advantage to institutional
 *   actors (courts, legislatures, platforms) and to political speakers while
 *   bearing costs on commercial, artistic, and marginalized speakers whose
 *   expression is reclassified as non-political when threatening. The reading
 *   is ONE of five sibling readings of the contested speech protection
 *   kernel; the reading-family relationship is modeled via cs_structure
 *   fields and omega variables.
 *
 * KEY AGENTS:
 *   - political_speakers_and_movements — primary beneficiaries; receive categorical protection
 *   - democratic_self_governance_institutions — agenda-setters; enforce and implement the tiered regime
 *   - non_political_expression_speakers — payers; bear reduced protection and regulatory burden
 *   - marginalized_political_movements — occupies precarious dual position; theoretically protected but reclassifiable when threatening
 *   - content_moderation_authorities — implement the hierarchy through enforcement decisions
 *   - competing_speech_theory_adherents — excluded; their alternative readings are subordinated to democratic-participation frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.31).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.28).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Democratic Participation Speech Protection Hierarchy").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '00e50cbd-4354-44a7-b48b-025b301a0d9b').
narrative_ontology:cs_kernel_codification('00e50cbd-4354-44a7-b48b-025b301a0d9b', fixed_text).
narrative_ontology:cs_authority_grounding('00e50cbd-4354-44a7-b48b-025b301a0d9b', lineage).
narrative_ontology:cs_interpretation_layer_present('00e50cbd-4354-44a7-b48b-025b301a0d9b').
narrative_ontology:cs_reading_relation('00e50cbd-4354-44a7-b48b-025b301a0d9b', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('00e50cbd-4354-44a7-b48b-025b301a0d9b', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('00e50cbd-4354-44a7-b48b-025b301a0d9b', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('00e50cbd-4354-44a7-b48b-025b301a0d9b', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_axiom('00e50cbd-4354-44a7-b48b-025b301a0d9b', foundational, political_speech_foundational_democracy).
narrative_ontology:cs_axiom_status(political_speech_foundational_democracy, holdable).
narrative_ontology:cs_axiom_grounding('00e50cbd-4354-44a7-b48b-025b301a0d9b', political_speech_foundational_democracy, deontological).
narrative_ontology:cs_axiom('00e50cbd-4354-44a7-b48b-025b301a0d9b', foundational, tiered_protection_necessary_coordination).
narrative_ontology:cs_axiom_status(tiered_protection_necessary_coordination, holdable).
narrative_ontology:cs_axiom_grounding('00e50cbd-4354-44a7-b48b-025b301a0d9b', tiered_protection_necessary_coordination, instrumental).
narrative_ontology:cs_reference_frame('00e50cbd-4354-44a7-b48b-025b301a0d9b', political_speech_democratic_necessity).
narrative_ontology:cs_drift_state('00e50cbd-4354-44a7-b48b-025b301a0d9b', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('00e50cbd-4354-44a7-b48b-025b301a0d9b', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_speakers_and_movements).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, citizens_accessing_political_discourse).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, democratic_self_governance_institutions).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, non_political_expression_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, speakers_facing_content_moderation_hierarchy).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_and_artistic_speech_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, marginalized_political_movements).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, marginalized_political_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political speakers—candidates, advocacy organizations, protest movements, editorial voices—receive maximal protection under this reading. Their speech is presumed necessary for democratic participation and receives categorical or near-categorical constitutional protection. They benefit from a structural framework that treats political expression as foundational and constraints on it as presumptively unconstitutional.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_speakers_and_movements, beneficiary,
    organized, generational, mobile, national).

% Citizens benefit from a legal regime that keeps political discourse open and robust. The reading protects their access to political information and debate necessary for informed electoral and civic participation. They experience this constraint as enabling democratic self-governance.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, citizens_accessing_political_discourse, beneficiary,
    organized, biographical, mobile, national).

% Legislatures, courts, election authorities, and the formal apparatus of democratic governance operate under this constraint, which subordinates speech regulation to the requirement that political discourse remain maximally protected. This reading empowers democratic institutions to defend political speech while restricting non-political speech more readily—establishing a tiered protection regime.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, democratic_self_governance_institutions, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, democratic_self_governance_institutions, agenda_setter).

% Commercial speech, artistic expression, intimate communication, and other non-political expression receive lower protection under this reading. Speakers in these categories bear the cost of a tiered system: the law treats their expression as more readily subject to regulation (for obscenity, fraud, incitement to private violence, harassment, intellectual property, etc.). They pay through reduced legal protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, non_political_expression_speakers, payer,
    moderate, biographical, constrained, national).

% Individuals whose expression falls outside political categories—personal opinion on non-political topics, intimate sharing, user-generated content—face an internalized hierarchy where platforms and regulators treat their speech as less deserving of protection. They experience suppression from the constraint's operation through content moderation systems trained on the tiered logic: political is sacred, other is negotiable.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, speakers_facing_content_moderation_hierarchy, payer,
    powerless, immediate, identity_locked, global).

% Advertising, entertainment, news-entertainment, commercial media, and artistic expression operate in a lower protection tier. These sectors bear regulatory burden and legal uncertainty: they can be restricted in ways political speech cannot. They navigate around the tiered constraint through litigation risk and content strategy adjustment.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, commercial_and_artistic_speech_sectors, payer,
    powerful, generational, arbitrage, global).

% Platform moderators, broadcast regulators, and administrative agencies enforce the hierarchy by differentially protecting political speech and constraining non-political expression. They set the line between political and non-political, implementing the tiered regime through enforcement decisions. This power is exercised continuously but the line itself remains contestable.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, content_moderation_authorities, agenda_setter,
    institutional, biographical, constrained, global).

% Groups whose political speech is not recognized as 'political' within dominant institutions—revolutionary speech, indigenous sovereignty claims, abolitionist rhetoric when abolition is non-mainstream—occupy a precarious position. They theoretically benefit from political-speech protection but often find their speech reclassified as non-political (incitement, sedition, harassment of targets) once its political content becomes threatening to the established order.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, marginalized_political_movements, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, marginalized_political_movements, payer).

% Courts and constitutional interpreters operationalize the democratic-participation reading by drawing boundaries between protected political speech and regulable non-political expression. They hold the power to expand or contract what counts as 'political' and therefore maximally protected. The constraint gives them this power; they exercise it through doctrine.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, court_and_legal_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for absolutist, dignity-based, marketplace-of-ideas, and harm-threshold readings of the speech protection kernel are excluded from steering this constraint. They would contest the hierarchy, expand protection categories, or reweight the protection calculus—but under the democratic-participation reading, their alternative frameworks are not recognized as equally legitimate interpretations of the constitutional commitment.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, competing_speech_theory_adherents, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__democratic_participation_reading, democratic_self_governance_institutions).
narrative_ontology:fixing_cost_class(speech_protection_kernel__democratic_participation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates democratic participation by ensuring political speech—the speech necessary for citizens to deliberate, organize, and hold power accountable—remains maximally protected. Solves the collective-action problem of how to sustain robust political discourse against regulatory pressure, market pressure, and institutional suppression. Creates a legal regime where the foundational speech for self-governance cannot be easily restricted.
% TRANSFER_FUNCTION: Transfers protection, stability, and legal advantage to political speakers and democratic institutions while bearing the cost of restricted protection for commercial, artistic, and intimate expression. Political speech receives categorical or near-categorical protection; non-political speech receives intermediate protection and can be regulated for commercial fraud, obscenity, harassment, incitement to violence, and other grounds that would fail if applied to political expression.
% ABSENT_VOICES: Absolutist speech-protection advocates (who would reject the tiering entirely), dignity-reading proponents (who would override political speech to prevent structural subordination), harm-threshold advocates (who would restrict political speech causing demonstrable harm), and marketplace theorists (who would trust correction-through-counter-speech rather than protection hierarchy). These readings remain live in academic and transnational contexts but are subordinated to the democratic-participation frame in American constitutional law.
% DISAPPEARANCE_RATIONALE: If this protection hierarchy disappeared—if political and non-political speech received equal legal protection, or non-political speech received equal protection—the regulatory environment would shift dramatically. Commercial regulation of speech (for fraud, deception, harassment) would become constitutionally suspect; content moderation of user speech would require heightened justification; political speech could be restricted on the same grounds as non-political expression. The institutional bias toward political protection would evaporate.
% FOUNDING_PROBLEM: Early democratic theory identified political discourse—speech necessary for citizens to understand government action, deliberate about policy, and organize political movements—as uniquely essential for self-governance. Restrictions on political speech were seen as threats to the democratic system itself, not merely threats to individual speakers. The hierarchy emerged to solve this: protect the speech democracy requires, while permitting regulation of expression that does not directly threaten democratic function.
% FOUNDING_PROBLEM_CORROBORATION: Democratic theorists from John Stuart Mill through contemporary scholars attest that political discourse is foundational for democracy. Courts cite this as the primary rationale for heightened political-speech protection. However, scholars in critical speech theory (Sunstein, Langton, Puar) contest whether the founding problem—whether political speech actually needs categorical protection—remains live, and whether the hierarchy actually solves it or masks subordination. Empirical research on suppression and false-flag distortion complicates the narrative that protection-without-hierarchy serves democracy better. External corroboration is mixed.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).
:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.31 at interval end) because the constraint does serve a genuine coordination function: protecting political discourse is necessary for democratic function and participants benefit from the stability of a regime that keeps political speech maximally protected. However, extraction rises over the interval (0.22→0.35) as institutional actors gain experience implementing the hierarchy and expand the zones where speech can be reclassified as non-political. Theater ratio is low-to-moderate (0.18) and rising (0.12→0.23), indicating that while the protection function is real, an increasing share of enforcement activity serves the reclassification machine rather than genuine speech protection. Suppression requirement is low (0.28) because the constraint operates through law (institutional interpretation) rather than raw coercion; however, suppression climbs as enforcement infrastructure develops. The time grid is shared across all three metrics at six time points (0, 8, 16, 25, 35, 50). The rising trajectory reflects the constraint's lifecycle: genuine coordination function in early stages (when the political/non-political boundary is clear and uncontested) shifts toward extraction as institutional actors gain experience instrumentalizing the boundary to suppress threatening-but-political speech. Accessibility_collapse at 0.62 reflects that once the democratic-participation reading is entrenched in constitutional law, speakers have few institutional exits (they operate within a legal regime the reading defines), but alternatives persist theoretically (competing readings). Resistance at 0.58 reflects moderate active contestation: the reading is not universally accepted, and alternative readings maintain scholarly and jurisdictional footholds.
 *
 * PERSPECTIVAL GAP:
 *   The democratic_self_governance_institutions and political_speakers_and_movements seats should compute as beneficiary-aligned; the constraint operates in their favor and they experience it as enabling their function. The non_political_expression_speakers and speakers_facing_content_moderation_hierarchy seats experience extraction: they see the same tiered regime as a constraint on their speech. The marginalized_political_movements seat occupies a structural contradiction: the reading offers them protection in principle but reclassification in practice. From the institutional seat, the hierarchy is a neutral coordination mechanism protecting necessary discourse. From the payer seats, it is a tool for suppressing non-dominant expression. The engine derives d per-seat from power + exit + beneficiary/victim declarations; the authored metrics (rising extraction, rising theater, moderate suppression) model the constraint's actual operation across these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Political speakers and democratic institutions are the beneficiaries (they structure the regime and benefit from the protection hierarchy) — d toward 0.0 (full beneficiary) for institutional agenda-setters (d ≈ 0.15–0.25), moderate-low d for organized political speakers (d ≈ 0.30–0.40). Non-political speakers are the payers: they face reduced protection and reclassification pressure — d toward 1.0 (full target) for powerless speakers with identity_locked exit (d ≈ 0.75–0.85), moderate d for powerful commercial sectors with arbitrage exits (d ≈ 0.50–0.60). Citizens accessing political discourse benefit from the robustness it ensures — d near symmetric (d ≈ 0.45–0.55). Marginalized political movements occupy the structural contradiction (theoretically beneficiary, practically payer) — d near symmetric but volatile, reclassification-sensitive (d ≈ 0.45–0.65 depending on whether speech is recognized as political). Competing-reading adherents are excluded, not classified — their structural position is institutional outsider, not payer or beneficiary within this constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (political discourse necessary for democracy) remains live in the sense that robust political discourse is still necessary for democratic function. However, the research question animating the tiering—whether categorical protection is necessary and whether the hierarchy actually achieves it—is increasingly contested. Critical scholarship (Sunstein, Puar, Langton) argues that the hierarchy does not solve the founding problem; instead, it serves institutional convenience by allowing regulation of marginalized and threatening speech under the rubric of non-political. If this scholarship is correct, the constraint exhibits mandatrophy: the founding problem (need for robust political discourse) remains live, but the solution (tiered protection) is increasingly disconnected from the problem. The regime persists not because it solves the founding problem more effectively than alternatives but because it is institutionally entrenched and benefits institutional actors. This is piton-adjacent: coordination function atrophied, but institutional maintenance continues. Omega variables documenting the hierarchy-necessity question and the marginalized-speech-reclassification pattern directly address this mandatrophy risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the democratic-participation reading the operative reading of the speech protection kernel, or do competing readings (absolutist, harm-threshold, dignity-based, marketplace) hold equal legitimacy within contemporary constitutional law?',
    'Citation analysis of Supreme Court opinions, legislative testimony, and constitutional scholarship; analysis of which reading governs judicial outcomes in specific cases (political vs. non-political). If competing readings produce contradictory holdings in structurally similar cases, no single reading is fully operative.',
    'If the democratic-participation reading is fully operative, the hierarchy is a binding constraint on all institutional interpretation. If competing readings hold equal force, the hierarchy is contested and its persistence depends on political-institutional choice rather than structural inevitability. This shifts classification from rope toward tangled_rope (contested legitimacy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which reading of the speech protection kernel actually governs institutional practice?').

omega_variable(
    political_nonpolitical_boundary,
    'What counts as ''political speech'' for purposes of heightened protection? Is the boundary stable, or is it continuously contested and reclassified by institutional actors?',
    'Systematic analysis of judicial boundary-drawing in cases where speech is classified as political or non-political. Track whether marginalized movement speech, commercial-political hybrid speech, or intimate-political speech is classified consistently or reclassified based on institutional threat assessment.',
    'If the boundary is contestable and reclassified to suppress threatening speech, the constraint functions as a snare (coercive suppression disguised as protection hierarchy) rather than a rope (genuine coordination). If the boundary is stable and reflects pre-institutional categorization, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_nonpolitical_boundary, empirical, 'Whether the political/non-political boundary is fixed or malleable to serve institutional power.').

omega_variable(
    hierarchy_necessity,
    'Is the tiered protection regime (political maximally protected, non-political restricted) structurally necessary for democratic participation, or do alternative regimes (equal protection across categories, or protection weighted by different factors) serve democratic function equally or better?',
    'Comparative constitutional law analysis; natural experiments from jurisdictions that adopt alternative speech regimes (equal protection, harm-based, dignity-based) and observe outcomes for democratic participation, dissent, and institutional stability. Theoretical analysis of whether the hierarchy actually prevents the problem it was designed to solve.',
    'If the hierarchy is not necessary and alternatives produce equivalent or superior outcomes, the constraint reclassifies from rope (serving genuine coordination function) to piton (inertial institutional maintenance without functional necessity) or tangled_rope (coordination function shadowed by extraction benefit for institutional actors).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hierarchy_necessity, conceptual, 'Whether the tiered protection hierarchy is structurally necessary or institutionally contingent.').

omega_variable(
    marginalized_speech_reclassification,
    'Is marginalized political speech (revolutionary, abolitionist, indigenist, anti-state) genuinely protected under the democratic-participation reading, or is it systematically reclassified as non-political (incitement, sedition, harassment) when it becomes institutionally threatening?',
    'Historical analysis of reclassification patterns; statistical analysis of speech classified as political vs. non-political by speaker identity and movement threat level; post-mainstreaming review of previously-suppressed speech (abolition, civil rights, LGBTQ+ movement speech) to assess whether earlier reclassification was structural or ideological.',
    'If marginalized political speech is systematically reclassified when threatening, the constraint functions as a snare for subordinated groups (protection conditional on non-threat) rather than a rope (genuine coordination). This would suggest the constraint vindicates the dignity_reading''s claim that speech protection cannot ignore subordination effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_speech_reclassification, empirical, 'Whether the democratic-participation reading''s protection extends to threatening political speech or is conditioned on institutional acceptability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t8, speech_protection_kernel__democratic_participation_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__democratic_participation_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__democratic_participation_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(spee_tr_t35, speech_protection_kernel__democratic_participation_reading, theater_ratio, 35, 0.2).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__democratic_participation_reading, theater_ratio, 50, 0.23).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(spee_be_t8, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement(spee_be_t35, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 35, 0.33).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.24).
narrative_ontology:measurement(spee_su_t8, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 16, 0.27).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement(spee_su_t35, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 35, 0.29).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 50, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__democratic_participation_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the contested speech protection kernel. The democratic-participation reading establishes a tiered regime (political speech maximally protected, non-political restricted) grounded in self-governance theory. Sibling readings reject the tiering in different ways: absolutist permits no tiering, harm-threshold overrides tiering for demonstrably harmful speech, dignity-based overrides tiering to prevent subordination, marketplace-based relies on correction-through-counter-speech rather than protection hierarchy. Each reading instantiates a structurally distinct constraint with different ε values and stakeholder positions. The readings coexist (with varying institutional strength) in contemporary constitutional law. Link all five stories via network.affects_constraints to model the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, powerless, 0.78).
constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
