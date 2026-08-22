% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Orthographic Legitimacy via Literacy Maximization (Instrumentalist Reading)
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   The orthographic legitimacy kernel is a contested commitment that grounds
 *   orthographic choice in one of three incompatible normative claims:
 *   preservation of tradition (continuity reading), maximization of access
 *   and efficiency (instrumentalist reading), or alignment with Western
 *   modernity (modernist reading). This constraint instantiates the
 *   INSTRUMENTALIST READING: script choice is justified by its capacity to
 *   maximize literacy rates and administrative efficiency, treating
 *   orthography as a pragmatic tool rather than a cultural or civilizational
 *   marker. The primary beneficiary is the newly literate population (whose
 *   participation the state enables via simplified, standardized script); the
 *   primary victim is the Arabic-literate elite (whose devalued expertise is
 *   the cost of the reform). The state apparatus functions as the
 *   agenda-setter, using efficiency metrics and literacy data to frame
 *   legitimacy and exclude competing framings—particularly the continuity
 *   reading's claims about literary heritage and religious fidelity. The
 *   measurement series track the constraint's maturation: extraction rises
 *   and plateaus as the reform cohorts graduate, theater rises as the state
 *   must increasingly defend the reform against religious and literary
 *   objections using performance metrics rather than new substance, and
 *   suppression requirement rises and stabilizes as the elite's ability to
 *   compete using the old script is systematically closed.
 *
 * KEY AGENTS:
 *   - Administrative State: Agenda-setter; designs and enforces the orthographic mandate; frames legitimacy via literacy statistics and administrative efficiency; directly benefits from reduced communication friction.
 *   - Newly Literate Population: Beneficiary; gains access to literacy and administrative participation through state-mandated orthography; formerly excluded by high cost of traditional script.
 *   - Arabic-Literate Elite: Victim/Payer; bearers of cost; specialized literacy skills devalued; constrained exit (state monopoly on legitimacy).
 *   - Religious Scholars: Payer + Excluded; identity-locked (professional authority constituted through mastery of classical texts); structurally absent from the instrumentalist discourse.
 *   - Traditional Literary Class: Excluded; would argue for preservation of canonical works; reframed as 'elitist' or 'nostalgic' by state rationalists.
 *   - State Rationalists: Beneficiary (analytical seat); frame debate instrumentally; occupy policy positions; treat script choice as optimization problem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.62).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.58).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Orthographic Legitimacy via Literacy Maximization (Instrumentalist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, 'b6a13979-4a50-4672-a7a8-a3f99618a743').
narrative_ontology:cs_kernel_codification('b6a13979-4a50-4672-a7a8-a3f99618a743', formalized).
narrative_ontology:cs_authority_grounding('b6a13979-4a50-4672-a7a8-a3f99618a743', extraction).
narrative_ontology:cs_interpretation_layer_present('b6a13979-4a50-4672-a7a8-a3f99618a743').
narrative_ontology:cs_reading_relation('b6a13979-4a50-4672-a7a8-a3f99618a743', orthographic_legitimacy_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6a13979-4a50-4672-a7a8-a3f99618a743', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_axiom('b6a13979-4a50-4672-a7a8-a3f99618a743', foundational, orthographic_legitimacy_derives_from_instrumental_efficacy).
narrative_ontology:cs_axiom_status(orthographic_legitimacy_derives_from_instrumental_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('b6a13979-4a50-4672-a7a8-a3f99618a743', orthographic_legitimacy_derives_from_instrumental_efficacy, empirically_contingent).
narrative_ontology:cs_axiom('b6a13979-4a50-4672-a7a8-a3f99618a743', secondary, literacy_maximization_supersedes_cultural_continuity).
narrative_ontology:cs_axiom_status(literacy_maximization_supersedes_cultural_continuity, holdable).
narrative_ontology:cs_axiom_grounding('b6a13979-4a50-4672-a7a8-a3f99618a743', literacy_maximization_supersedes_cultural_continuity, conventional).
narrative_ontology:cs_reference_frame('b6a13979-4a50-4672-a7a8-a3f99618a743', fragmented_pre_reform_orthography).
narrative_ontology:cs_drift_state('b6a13979-4a50-4672-a7a8-a3f99618a743', post_reform_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b6a13979-4a50-4672-a7a8-a3f99618a743', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, administrative_state).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, educators).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_rationalists).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, religious_scholars).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces orthographic reform to maximize literacy rates and administrative efficiency. Allocates education budgets, mandates script instruction in schools, and incorporates the chosen script into all official documentation. Justifies the reform through literacy statistics, enrollment numbers, and efficiency metrics of the bureaucracy. Directly benefits from reduced communication friction and wider access to administrative services.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, administrative_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains access to literacy and administrative participation through the state-mandated orthography. For populations previously excluded from literacy due to high cost of learning the traditional script, the reform lowers the barrier to entry. They benefit from state schooling, employment opportunities in expanding administrative structures, and reduced communication friction. Their exit option is constrained by the state's monopoly on legitimate education and official communication channels.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    organized, biographical, constrained, national).

% Bears the cost of orthographic devaluation: their specialized literacy skills, acquired at high cost, lose institutional and economic value as the state privileges the new script. They face pressure to relearn or accept marginalization from administrative and literary authority structures. Though institutionally powerful, their exit options are constrained by state monopoly on legitimacy and the irreversibility of the reform once enrollment cohorts graduate in the new script.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite, payer,
    powerful, generational, constrained, national).

% Bears the cost of potential rupture from religious textual tradition if the orthography chosen cannot represent religious language with the same fidelity as the displaced script. Their identity is constituted through mastery of classical religious texts; exit from the constraint would require abandoning their professional and spiritual authority. They are excluded from the literacy-maximization discourse—the state and newly literate population frame the question as efficiency and access, not fidelity to tradition.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, religious_scholars, payer,
    powerful, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, religious_scholars, excluded).

% Benefit from expanded employment and institutional resources as the state builds mass education infrastructure around the new orthography. They also bear a cost: retraining themselves in the new script and pedagogy, potentially devaluing their existing expertise. Their career pathways are now organized around the state's mandate; exit means leaving education entirely.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, educators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, educators, payer).

% Occupy analytical and policy seats inside the state apparatus. They advocate for the reform via efficiency metrics, cost-benefit analysis, and literacy data. They treat script choice as a tool to be optimized and frame the debate in instrumental terms—maximizing human capital and administrative throughput. They have analytical rather than material stake in the outcome.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_rationalists, beneficiary,
    institutional, generational, analytical, national).

% Would argue that literary legitimacy derives from preservation of canonical works written in the displaced script, and that access-via-new-script means access-to-translations, not access-to-source. They are structurally excluded from the instrumentalist discourse because the state and newly literate population do not frame the question as 'which script preserves literature' but as 'which script maximizes access.' Their authority is not consulted; their objections are reframed as nostalgia or elitism by the state rationalists.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, traditional_literary_class, excluded,
    powerful, civilizational, identity_locked, national).

% Might advocate for alternative orthographies that could also maximize literacy while preserving more continuity with tradition. They are excluded because the state's monopoly on legitimacy means their proposals cannot compete once the state commits to a single reform path. Inclusion would change the framing from 'which script the state mandates' to 'which orthography best serves these multiple goals.'
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, competing_script_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, administrative_state).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: unified script enables mass literacy (lower per-capita education cost), reduces communication friction in administration and commerce, and creates a common namespace for bureaucratic coordination. Without orthographic standardization, literacy remains high-barrier, segmented by local scripts, and administrative efficiency depends on translators and intermediaries.
% TRANSFER_FUNCTION: Moves institutional authority, economic value, and cultural prestige from the Arabic-literate elite (whose skills are devalued) to the newly literate population (whose access is enabled) and to the administrative state (which captures efficiency gains). In a narrower sense: transfers the cost of learning from the state to the Arabic-literate elite (who must relearn or be marginalized).
% ABSENT_VOICES: Religious scholars and traditional literary classes are structurally excluded from the discourse. They would frame the question as 'which script preserves our civilization's written heritage' rather than 'which script maximizes literacy rates.' Their objections are reframed as resistance to progress, not as legitimate competing claims about orthographic legitimacy. Competing script advocates are also excluded: once the state commits to one reform path, alternatives are no longer entertained within the state apparatus.
% DISAPPEARANCE_RATIONALE: If the orthographic mandate vanished, the newly literate population would lose the state education pathway that enabled their literacy. The devalued elite would recover institutional authority. Administrative efficiency would return to requiring translators and specialized personnel. The constraint's persistence depends on active state enforcement—without it, market and cultural forces would reassert the older script's value (carriers of literary, religious, and elite tradition would retain authority). The arrangement does not self-sustain.
% FOUNDING_PROBLEM: Low literacy rates and administrative inefficiency caused by fragmented, high-barrier orthographies; communication friction between literate and non-literate populations; state inability to coordinate bureaucratic operations across linguistic variants.
% FOUNDING_PROBLEM_CORROBORATION: The administrative state attests the problem is live and the reform solves it, citing literacy enrollment and administrative throughput data. Comparative historians and educational economists outside the state (and not party to the beneficiary class) independently attest that literacy expansion correlates with script simplification and standardization—cross-national evidence from similar state formation contexts. Religious scholars and literary traditionalists attest the problem is real but argue the solution chosen was unnecessarily destructive to cultural continuity; their corroboration is qualified by their status as payers.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.62 at interval end) because the constraint operates as genuine coordination—solving a real collective-action problem (unified script enables mass literacy)—but with substantial asymmetric cost to the devalued elite. The gain (newly literate population) and the loss (Arabic-literate elite) are real and material, not mere redistribution of prestige. Suppression is moderate-high (0.58) because the constraint's persistence depends on actively preventing the elite from reasserting the old script's value (via excluding them from the discourse, using state education monopoly to train new cohorts, and delegitimizing objections as backward). Theater is moderate (0.41) and rising through the interval: early in the reform, the state's case is purely efficiency-and-literacy (low theater); as literacy plateaus and the elite continues to object, the state must perform the efficiency claim more theatrically—producing literacy metrics, efficiency reports, comparative studies—without being able to add new substance to the justification. Accessibility collapse is moderate (0.48): alternatives do collapse (if you accept the state's framing, the new script is the only rational choice), but they do not collapse completely because competing readings remain intellectually coherent and are actively maintained by excluded parties. Resistance is substantial (0.62): the constraint meets real resistance from religious scholars, literary traditionalists, and the devalued elite; this is not a mountain with near-zero resistance but an enforced arrangement that people actively resist. The measurement trajectory shows extractiveness rising fast (0-20) then plateauing (20-40): the reform's benefits front-load to the newly literate (who are fastest to enroll), while the elite's losses are sustained and don't deepen further—they've already lost their monopoly. Theater rises throughout (18→41): the constraint's legitimation narrative must work harder as immediate material benefits plateau. Suppression requirement also rises and plateaus: the state must continuously prevent the elite from reasserting the old script; once enforcement infrastructure is in place, additional suppression is maintenance, not escalation.
 *
 * PERSPECTIVAL GAP:
 *   The state rationalist seat and the payer seats compute fundamentally differently. From the state's position, the arrangement is pure rope: it solves genuine coordination (unified literacy), benefits all participants (the newly literate by definition; the state by efficiency), and is justified by transparency (efficiency metrics, literacy data). From the Arabic-literate elite seat and the religious scholar seat, the same structure operates as tangled rope or snare: they are compelled to participate in a system that devalues their expertise and is sustained by state enforcement of the new script's monopoly. The elite cannot exit because the state monopolizes legitimacy; religious scholars cannot exit because their identity is locked to classical texts (exiting means ceasing to be scholars). The newly literate population seat is genuinely beneficiary-aligned with the state's framing, but they benefit precisely because the elite bears a cost—the arrangement is parasitic on skill devaluation. The engine will compute per-seat types from the authorized structural data; the divergence is the central analytical fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is the agent's structural relationship to THIS SPECIFIC constraint. The administrative state is the beneficiary (d ≈ 0.1–0.2: the constraint subsidizes the state by delivering coordination benefits and efficiency gains). The newly literate population is a beneficiary (d ≈ 0.1–0.3: they gain access without running the constraint; their directionality is slightly higher than the state's because they also bear diffuse cost if the state mis-allocates education resources). The Arabic-literate elite are targets (d ≈ 0.75–0.85: the constraint extracts from them—their skills are devalued—and they cannot exit because the state monopolizes legitimacy; trapped/identity_locked exit multiplies their effective extraction). Religious scholars are also targets (d ≈ 0.80: identity-locked exit; if they leave the constraint system, they cease to be scholars; the constraint forces them to either relearn in the new script or accept marginalization). Educators sit near symmetric (d ≈ 0.45–0.55: they benefit from expanded employment but bear cost of retraining; constrained exit means they absorb both sides). State rationalists are beneficiaries (d ≈ 0.15: they occupy analytical seats, gain prestige from being the constraint's architects, and have arbitrage-grade exit if needed). No directionality override is needed—the structural derivation captures the asymmetry cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy through two mechanisms. First, the founding problem (low literacy, administrative inefficiency) remains live—the state and comparative evidence both attest to this. The measurement series does not show the constraint losing its coordination function; literacy is genuinely expanding and administrative efficiency is genuinely improving (extractiveness plateaus because the gains front-load, not because they evaporate). Second, the beneficiary class (newly literate population) is actively growing—new cohorts of state-educated children represent a constituency that benefits from and will defend the constraint. The devalued elite are aging and their descendants are trained in the new script. Mandatrophy would occur if the founding problem died (literacy became universal without script reform) while the constraint persisted—but that does not fit this case. The rising theater ratio (state must perform the efficiency claim more theatrically as time goes on) is a warning signal: it indicates the constraint's ability to justify itself through new material evidence is weakening, and it is increasingly relying on performative legitimation. But theater is not yet above 0.50, so the constraint still has real function beneath the performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_vs_access_distinction,
    'Does the instrumentalist reading conflate literal literacy (ability to decode symbols) with effective access (ability to participate in institutions using the new script)? Are newly literate individuals who learn the state script genuinely more able to participate, or do they require additional gatekeeping (exam systems, credentials, institutional affiliation)?',
    'Longitudinal data on employment, administrative participation, and income outcomes for newly literate cohorts vs. the pre-reform Arabic-literate population; cross-sectional analysis of institutional gatekeeping mechanisms that remain even after script literacy is achieved.',
    'If access is substantially gated by non-script mechanisms (exams, credentials, institutional closure), the constraint''s legitimation narrative (literacy → participation) is partly fiction. The measured extraction would be accurate but the claimed coordination function would be partially illusory—the constraint achieves literacy expansion but not genuine access expansion. This would shift classification toward tangled_rope or snare from the newly literate population''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_vs_access_distinction, empirical, 'Whether the constraint''s legitimating claim about access is empirically true or relies on slippage between literacy and institutional participation.').

omega_variable(
    script_choice_optimality,
    'Is the chosen script optimal for literacy maximization and administrative efficiency, or was it selected for political reasons (alignment with a dominant foreign power, suppression of minority languages, assertion of state authority) and then justified post-hoc via efficiency claims?',
    'Comparative orthographic analysis: would an alternative script (e.g., phonetically more regular, closer to spoken language, preserving more continuity with tradition) have yielded equal or greater literacy and efficiency gains? Archival evidence of the script selection process (was it evidence-based or politically motivated?). Cross-national comparison with other script reforms and their efficacy.',
    'If the script was chosen for political rather than efficiency reasons, the constraint''s legitimacy rests on a false foundational claim. The instrumentalist framing becomes pure theater—a narrative imposed after the fact to justify a politically motivated choice. Extraction would be recharacterized as conscious imposition, not genuine problem-solving. Type would shift toward snare from the elite''s perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(script_choice_optimality, empirical, 'Whether the script choice was actually optimized for efficiency or justified post-hoc to cover political motivation.').

omega_variable(
    sibling_reading_foreclosure,
    'Are the instrumentalist and continuity readings logically foreclosed from coexisting within a single framework, or do they occupy genuinely different institutional contexts (state administration vs. religious/literary authority) and could both persist in a layered system?',
    'Examination of jurisdictions or time periods where both readings were institutionally live simultaneously: e.g., state schools teaching the new script while religious institutions maintained the old script for canonical texts. If such layering is possible without framework collapse, foreclosure is not necessary.',
    'If foreclosure is not necessary (readings can coexist in different institutional contexts), then the constraint''s zero-sum framing is a choice by the state to eliminate the continuity reading rather than a structural necessity. The extraction from the Arabic-literate elite is then more purely political—a choice to consolidate state power over religious and literary authority—rather than an unavoidable coordination cost. Type remains rope if the state''s framing of necessity is accepted; shifts toward snare if the alternative framings are understood as suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the instrumentalist and continuity readings are logically incompatible or could coexist in a layered institutional structure.').

omega_variable(
    suppression_vs_structural_collapse,
    'Does the measured suppression represent active state enforcement preventing the elite from using the old script, or does it represent structural collapse of the old script''s institutional supports (religious institutions, literary guilds, merchant networks) independent of state suppression?',
    'Evidence from instances where the state did NOT actively suppress the old script (e.g., private religious schools, merchant correspondence, literary circles): did these institutions persist or collapse? Comparison with script transitions where the state was neutral or permissive.',
    'If suppression is the causal driver, the constraint is actively enforced and depends on continuous state action—removing it would restore the old script relatively quickly. If structural collapse is the driver, the old script''s eclipse is path-dependent but not solely state-caused—market and demographic forces have made it obsolete. The former supports piton classification (dependent on state theater), the latter supports rope classification (genuinely self-sustaining once initial transition passes). The measured suppression value is identical in either case; the interpretation changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_structural_collapse, empirical, 'Whether measured suppression is active enforcement or structural inevitability.').

omega_variable(
    reading_versus_constraint_identity,
    'Is this constraint best understood as an instance of the READING (the instrumentalist interpretation of orthographic legitimacy as such) or as an instance of the STANDING ARRANGEMENT (the specific script reform the state chose)? If the state later adopted the same script but justified it via continuity or modernist readings instead, would that be a different constraint or the same constraint read differently?',
    'Conceptual clarification: does the constraint''s identity ride on the specific justification (instrumentalist reading) or on the material arrangement (script reform)? A reading-invariant constraint would have a stable ε and similar classification under different readings; a reading-dependent constraint would shift ε when the reading changed. Examine whether switching readings while holding the standing arrangement constant alters which parties are beneficiaries/victims.',
    'This is a meta-omega about the schema itself: it clarifies what ''this constraint'' refers to. If reading-dependent, then the corpus will contain three separate constraints (one per reading) that share the same material arrangements but have different beneficiaries/victims, ε values, and classifications. If reading-invariant, the standing arrangement has a stable ε and the three readings are just different justifications for the same underlying extraction/coordination. The corpus strategy (separate files or shared file with variant interpretations) depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_constraint_identity, conceptual, 'Whether this constraint''s identity is tied to the instrumentalist reading or to the underlying material arrangement independent of reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t5, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement_basis(orth_tr_t5, observed).
narrative_ontology:measurement(orth_tr_t10, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(orth_tr_t10, observed).
narrative_ontology:measurement(orth_tr_t15, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(orth_tr_t15, observed).
narrative_ontology:measurement(orth_tr_t20, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(orth_tr_t20, observed).
narrative_ontology:measurement(orth_tr_t25, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(orth_tr_t25, observed).
narrative_ontology:measurement(orth_tr_t30, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(orth_tr_t30, observed).
narrative_ontology:measurement(orth_tr_t40, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(orth_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t5, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(orth_be_t5, observed).
narrative_ontology:measurement(orth_be_t10, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(orth_be_t10, observed).
narrative_ontology:measurement(orth_be_t15, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(orth_be_t15, observed).
narrative_ontology:measurement(orth_be_t20, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(orth_be_t20, observed).
narrative_ontology:measurement(orth_be_t25, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(orth_be_t25, observed).
narrative_ontology:measurement(orth_be_t30, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(orth_be_t30, observed).
narrative_ontology:measurement(orth_be_t40, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(orth_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(orth_su_t0, observed).
narrative_ontology:measurement(orth_su_t5, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(orth_su_t5, observed).
narrative_ontology:measurement(orth_su_t10, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(orth_su_t10, observed).
narrative_ontology:measurement(orth_su_t15, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(orth_su_t15, observed).
narrative_ontology:measurement(orth_su_t20, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(orth_su_t20, observed).
narrative_ontology:measurement(orth_su_t25, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(orth_su_t25, observed).
narrative_ontology:measurement(orth_su_t30, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(orth_su_t30, observed).
narrative_ontology:measurement(orth_su_t40, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(orth_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__instrumentalist_reading, 0.18).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% The orthographic legitimacy kernel decomposes into three structurally distinct constraints, one per reading. Each reading instantiates a different beneficiary/victim structure and a different ε value. The standing arrangement (script reform) is shared; the readings (continuity, instrumentalist, modernist) are incompatible justifications held by different institutional parties. This constraint represents the instrumentalist reading: orthographic legitimacy derives from maximizing literacy rates and administrative efficiency. The continuity reading grounds legitimacy in preservation of access to tradition (separate constraint file). The modernist reading grounds legitimacy in alignment with Western modernity and rupture from Ottoman/Islamic past (separate constraint file). The three readings coexist as live positions held by different seats (state rationalists favor instrumentalist, religious scholars favor continuity, Western-aligned elites favor modernist). The engine will compute classifications per-seat from the authorized structural data of each reading separately; the network links document the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
