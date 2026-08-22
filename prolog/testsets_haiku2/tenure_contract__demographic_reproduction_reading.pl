% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_demographic_reproduction, []).

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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   Academic tenure systems were founded to protect scholars from
 *   institutional retaliation for controversial inquiry—the academic-freedom
 *   reading. But under the demographic-reproduction reading, the same
 *   gatekeeping mechanism operates to preserve institutional demographic
 *   composition by applying discretionary 'fit' and 'collegiality' standards
 *   that align with dominant-group norms. Underrepresented scholars face
 *   higher evaluation thresholds, penalization for challenging paradigms, and
 *   identity-fusion barriers that prevent exit even when they experience
 *   disadvantage. The constraint thus coordinates protection of inquiry while
 *   simultaneously extracting advancement opportunity from underrepresented
 *   groups and transferring it to dominant faculty. This is a genuine tangled
 *   rope: real coordination function (academic freedom) yoked to asymmetric
 *   extraction (demographic gatekeeping).
 *
 * KEY AGENTS:
 *   - Demographically dominant faculty: institutional power, arbitrage exit (can move between institutions or fields while retaining advantage); define and apply gatekeeping criteria; benefit from preferential evaluation.
 *   - Underrepresented scholars: moderate power, identity-locked exit (exit signals self-negation); face higher thresholds and 'collegiality' penalties; bear the extraction.
 *   - Women in STEM and scholars of color: specific underrepresented cohorts; face amplified evaluation burdens and double-binds; experience higher attrition.
 *   - Tenure committees: institutional power, analytical exit; wield official evaluation authority; comprise senior faculty drawn from existing composition; apply discretionary standards defended as meritocratic.
 *   - Institutional administration: institutional power, analytical exit; holds formal policy authority but delegates to faculty committees; responds to diversity pressure with symbolic reform (rubrics, training) that does not change underlying gatekeeping.
 *   - Underrepresented student populations: powerless, trapped exit; theoretically benefit from faculty representation but gatekeeping ensures scarcity; exit decisions shaped by witnessing disadvantage.
 *   - External accreditors: institutional power, trapped exit; audit diversity metrics but lack authority to mandate evaluation reform; become theater stage for institutions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.78).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.81).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, '3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba').
narrative_ontology:cs_kernel_codification('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', formalized).
narrative_ontology:cs_authority_grounding('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', lineage).
narrative_ontology:cs_interpretation_layer_present('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba').
narrative_ontology:cs_reading_relation('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', tenure_contract__institutional_extraction_reading, influences).
narrative_ontology:cs_axiom('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', foundational, gatekeeping_as_primary_mechanism).
narrative_ontology:cs_axiom_status(gatekeeping_as_primary_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', gatekeeping_as_primary_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', foundational, demographic_reproduction_as_central_outcome).
narrative_ontology:cs_axiom_status(demographic_reproduction_as_central_outcome, holdable).
narrative_ontology:cs_axiom_grounding('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', demographic_reproduction_as_central_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', meritocratic_scholarly_evaluation).
narrative_ontology:cs_drift_state('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', contemporary_diversity_equity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3cb9c75b-62b1-4b00-ab98-ac1e57bc27ba', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_scholars).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, women_in_stem).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, scholars_of_color).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, underrepresented_student_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold majority positions in tenure committees and senior ranks. Define and apply 'fit' and 'collegiality' standards that align with their own disciplinary norms, communication styles, and research aesthetics. Benefit from preferential evaluation and advancement rates substantially higher than underrepresented peers. Their exit option is to move institutions or disciplines, but their advantage is sufficiently portable that they rarely face forced exit; when challenged on bias they can reorganize evaluation criteria to preserve outcomes while appearing responsive.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, beneficiary).

% Face consistent evaluation gaps: their research productivity and innovation are often rated as lower 'fit' when held to aesthetic or methodological standards set by dominant groups. 'Collegiality' assessments penalize those who speak up about bias or operate outside dominant social networks. Exit is theoretically available (move fields, pursue industry work) but is identity-fused: their identity as scholars is constituted through the academic path itself; leaving signals to themselves and others that they did not belong. Remain in the system despite disadvantage because departure is experienced as self-negation.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_scholars, payer,
    moderate, biographical, identity_locked, national).

% Undergo evaluation under 'communality' vs. 'agency' double-bind: assertiveness is penalized as unsociable; passivity is penalized as lacking leadership. Research on collaborative work gets less credit when the evaluator's mental model of 'authorship' is male-default. Advancement rates lag significantly despite equal or superior productivity metrics. Identity-locked by professional investment and by the specific-field expertise that does not transfer cleanly to other sectors.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, women_in_stem, payer,
    moderate, biographical, identity_locked, national).

% Face dual taxation: scholarship on race, identity, or non-Western contexts is evaluated as 'niche' or 'advocacy' rather than rigorous inquiry; scholarship in dominant-group topics is evaluated for 'fit' to existing paradigms, disadvantaging methodological or theoretical innovation. Service expectations (mentoring, committee work, representation) rise sharply, consuming time for research advancement. Exit faces the same identity-fusion barrier as other underrepresented groups, amplified by pipeline effects: departing signals that the institution's diversity commitment failed.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, scholars_of_color, payer,
    moderate, biographical, identity_locked, national).

% Wield the official authority to evaluate tenure candidates against stated research, teaching, and service criteria. Comprise senior faculty drawn from the institution's existing composition. Apply criteria that are nominally objective but admit substantial discretion in weighing dimensions like 'impact', 'promise', and 'collegiality'. The criteria are defended as protecting scholarly independence and institutional quality; their effect is to reproduce the demographic composition of the committee itself. Committees respond to external pressure (diversity mandates, hiring data scrutiny) by adding diversity language and metrics without changing underlying evaluation culture.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, tenure_committees, agenda_setter,
    institutional, generational, analytical, national).

% Holds formal authority over tenure policy, promotion criteria, and hiring targets, but delegates evaluation authority to faculty committees. Faces contradictory pressures: constituent satisfaction from dominant-group faculty (who benefit from current standards) and external accountability for diversity and equity goals. Responds with symbolic policy reforms—adding evaluation rubrics, diversity criteria, unconscious-bias training—that do not substantially change outcomes, because the underlying gatekeeping mechanism ('fit', 'collegiality') remains discretionary and faculty-controlled. Theatrically maintains that the system is meritocratic and colorblind.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, institutional_administration, observer,
    institutional, generational, analytical, national).

% Theoretically benefit from representation: seeing faculty that look like them improves recruitment, retention, and sense of belonging. But the gatekeeping mechanism ensures that representation remains scarce and that admitted scholars face visible disadvantage in evaluation. The constraint's operation teaches students that advancement in the field requires either assimilation to dominant norms or acceptance of structural disadvantage, in turn shaping their own exit decisions from the pipeline.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_student_populations, beneficiary,
    powerless, biographical, trapped, national).

% Are invited to audit diversity metrics and hiring practices but lack authority to mandate evaluation reform. Their audits become a theater stage: institutions produce demographic data and diversity initiatives; accreditors certify progress; the underlying gatekeeping culture remains intact because accreditation authority stops at institutional boundaries.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, external_accreditation_bodies, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Tenure systems coordinate protection of scholarly inquiry: they decouple researcher survival from institutional or political pressure, enabling high-risk investigation. But under the demographic-reproduction reading, this coordination function is captured: the gatekeeping mechanism protects demographic stability (the coordination problem solved is 'preserve institutional composition') rather than inquiry itself.
% TRANSFER_FUNCTION: Transfers advancement opportunity and long-term employment security preferentially to demographically dominant faculty while imposing higher evaluation thresholds and identity-fusion costs on underrepresented scholars. The mechanism is not monetary: the transfer is access to career paths, collegial networks, and the power to define what counts as good scholarship.
% ABSENT_VOICES: Underrepresented scholars inside the system are present but structurally subordinated: their objections to 'fit' criteria or 'collegiality' standards are themselves marked as failure to fit. Scholars who exited the pipeline (did not achieve tenure, departed academia) are completely absent from the evaluation conversation, even though their exit is itself evidence of gatekeeping. Community voices outside academia (students, practitioners, affected publics) are not seated in tenure committees.
% DISAPPEARANCE_RATIONALE: If tenure peer review gatekeeping vanished—replaced by transparent, outcome-blind evaluation focused only on research productivity, teaching effectiveness, and service—institutional demographics would shift rapidly within two to three promotion cycles. Dominant groups would experience lower advancement rates; underrepresented groups would advance at rates approaching productivity. The composition of knowledge-producing institutions would visibly change. The gatekeeping mechanism's removal would force resource and status redistribution.
% FOUNDING_PROBLEM: Academic inquiry is fragile in the face of institutional or political pressure. Scholars investigating controversial topics, challenging established paradigms, or pursuing risky methodologies face institutional retaliation (firing, denied promotion, defunded labs). Tenure protects this inquiry by decoupling employment security from institutional whim or ideological conformity.
% FOUNDING_PROBLEM_CORROBORATION: The academic-freedom framing (sibling reading) is attested by scholars who have fought off institutional pressure and by comparative international data showing that countries without tenure protections experience higher censorship of inquiry. The demographic-reproduction reading draws corroboration from independently published equity audits of tenure rates by demographic group, longitudinal data on 'fit' and 'collegiality' score distributions across demographic categories, and exit-interview data showing identity-fusion barriers as reasons scholars leave academia despite research productivity. Both readings cite the same institutional tenure structure; they interpret its effects differently based on whose experience is centered.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__demographic_reproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__demographic_reproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading authors high extractiveness (0.78 at interval end) because the constraint transfers advancement opportunity preferentially along demographic lines, decoupled from research productivity. Suppression is high (0.81) because the gatekeeping mechanism must be actively defended: underrepresented scholars question 'fit' criteria, produce counter-data on bias, organize equity initiatives. These challenges threaten the mechanism's naturalization, so the system responds with intensified enforcement (stricter criteria articulation, more robust 'collegiality' defense, mobilization of majority faculty to preserve standards). Theater ratio rises sharply from 0.38 to 0.62 over the interval: this reflects the proliferation of diversity language, equity rubrics, unconscious-bias training, and demographic auditing—all of which are performative maintenance of legitimacy rather than structural change. The underlying gatekeeping persists; the theater around it grows. The measurement series tracks a characteristic piton dynamic: extractiveness plateaus as the gatekeeping function stabilizes; theater rises as the system responds to visible critique with symbolic reform; suppression requirement continues to rise because the theater is insufficient to satisfy critics, so active maintenance intensifies. This is not a declining constraint; it is a constraint whose function has degraded (no longer primarily protecting inquiry) but whose gatekeeping persistence drives theater and suppression upward.
 *
 * PERSPECTIVAL GAP:
 *   Three seats with substantially different computed types: (1) demographically-dominant-faculty seat (d ≈ 0.2) likely computes as rope or tangled-rope-beneficiary because they experience genuine coordination benefit (inquiry protection) and their extraction is invisible to them (they define the standards as natural or inevitable). (2) underrepresented-scholars seat (d ≈ 0.85) likely computes as snare or tangled-rope-payer because they experience high extraction with suppression (gatekeeping barriers, penalty for deviation, internalized self-doubt) and limited exit. (3) institutional-administration seat (d ≈ 0.5) likely computes as tangled-rope-neutral or piton because they experience the constraint as a coordination problem they manage (protecting inquiry) but also as a governance burden they must defend (diversity pressure, liability risk), with no concentrated benefit accruing to them.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: demographically-dominant-faculty are declared beneficiaries (they collect advancement opportunity); underrepresented-scholars and women-in-stem are declared victims (they bear extraction). Beneficiary groups get d near 0.0 (full beneficiary end); victim groups get d near 1.0 (full target end). The exit-options modulation reinforces this: dominant faculty have arbitrage exit (can move institutions without losing relative advantage), driving d toward beneficiary end; underrepresented scholars have identity-locked exit (leaving signals failure to belong, and professional identity is constituted through the academic path), driving d toward target end. Power differentiation also reinforces: institutional power (committees, administrators) can reshape criteria; moderate power (candidate scholars) must accommodate criteria as given. The directionality overrides are not needed here because the structural derivation is tight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting controversial inquiry from institutional retaliation) remains live in specific domains: scholars studying contentious topics in political science, environmental science, and indigenous knowledge systems still face institutional pressure. But for the majority of tenure-track candidates, the founding problem has shifted: gatekeeping is now the dominant institutional pressure, not external ideology. The constraint persists in its inquiry-protection form, but its primary function has become demographic reproduction. This is a classic mandatrophy signature: the founding mandate (inquiry protection) and the primary operation (demographic gatekeeping) have diverged. The institutional response is theatrical: adding equity language and rubrics to a gatekeeping mechanism that remains fundamentally intact. The tangled-rope classification captures this better than piton would: there is real coordination (inquiry is genuinely more protected under tenure than under at-will employment), but the same mechanism also does substantial extraction (gatekeeping that reproduces demographic composition). Not pure piton (which would be mostly inert theater), but tangled rope where one of the yoked functions (gatekeeping) has grown to dominate the other (inquiry protection) in actual practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fit_criteria_naturalness,
    'Are ''fit'' and ''collegiality'' criteria evaluating genuine research or teaching merit, or are they proxies for demographic and cultural homogeneity?',
    'Conduct audit of ''fit'' ratings by demographic group while controlling for research productivity, publication impact, and teaching evaluations. If ''fit'' ratings diverge from productivity metrics in patterns correlated with demographic identity, the criteria are serving a gatekeeping function. Natural-language analysis of evaluation letters comparing language used for dominant-group vs. underrepresented candidates (agency vs. communality markers, evaluation of innovation vs. evaluation of conformity).',
    'If ''fit'' criteria are found to be demographic proxies, the tangled-rope classification holds with high confidence: the gatekeeping function is active and intentional. If ''fit'' ratings correlate perfectly with productivity, the constraint might be rope with minor implementation variation; gatekeeping would be incidental, not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fit_criteria_naturalness, empirical, 'Whether ''fit'' and ''collegiality'' standards measure research merit or enforce demographic conformity.').

omega_variable(
    kernel_reading_contest,
    'Does tenure''s primary structural function protect inquiry from institutional retaliation, or does it protect demographic composition from external diversity pressure?',
    'Trace history of tenure''s use: (1) How often is tenure invoked as a defense against institutional retaliation for controversial inquiry vs. as a justification for maintaining demographic composition in the face of hiring diversity initiatives? (2) What proportion of tenure denials cite ''lack of fit'' vs. ''inadequate research productivity''? (3) In institutions that have reformed tenure criteria (added explicit diversity considerations, blind review, outcome metrics), do demographic advancement rates change, or does gatekeeping resurface through new criteria?',
    'If tenure is primarily used to protect inquiry, the academic-freedom reading is primary; demographic stratification is incidental or caused by upstream pipeline factors. If tenure is primarily used to justify demographic gatekeeping against diversity pressure, the demographic-reproduction reading is primary; the constraint is tangled rope or snare. This directly determines the reading''s validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'What is the primary structural function tenure systems actually serve: inquiry protection or demographic preservation?').

omega_variable(
    gatekeeping_mechanism_reversibility,
    'If tenure-committee demographics shifted to majority-underrepresented composition, would evaluation standards reverse (underrepresented-friendly criteria becoming gatekeeping), or would standards converge on research productivity?',
    'Natural experiment: compare tenure evaluation culture in departments with majority-underrepresented faculty vs. majority-dominant faculty in the same institution. If gatekeeping mechanism reverses (underrepresented candidates face higher bars, dominant candidates face easier admission), the mechanism is inherently about demographic closure. If standards converge on productivity regardless of committee composition, gatekeeping is incidental.',
    'If the mechanism reverses, the gatekeeping function is structural and likely to persist even under demographic shifting—a high-confidence tangled-rope reading. If standards converge, the gatekeeping is a temporary artifact of institutional composition and might resolve through demographic change—a lower-confidence reading, closer to rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gatekeeping_mechanism_reversibility, empirical, 'Is the gatekeeping mechanism invariant to committee demographics or would it reverse under demographic shifting?').

omega_variable(
    alternative_reading_foreclosure,
    'Does the demographic-reproduction reading logically foreclose the academic-freedom reading, or can both be true (inquiry is protected AND gatekeeping occurs simultaneously)?',
    'Conceptual analysis: academic freedom is about institutional retaliation for ideas; demographic gatekeeping is about institutional homogeneity maintenance. An institution could theoretically protect inquiry (defend a scholar investigating controversial topics) while simultaneously gatekeeping (admit only scholars whose research aesthetics fit dominant norms). These are orthogonal mechanisms. However, if gatekeeping operates by penalizing unfamiliar research approaches as ''not serious inquiry'' or ''not rigorous,'' then the readings DO conflict: the same mechanism cannot simultaneously protect and suppress inquiry.',
    'If the readings coexist_with, both constraints are simultaneously true in different institutional contexts or for different scholar populations. If the reading forecloses academic-freedom (gatekeeping by redefining what counts as inquiry), the demographic-reproduction mechanism undermines the academic-freedom coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Logical relationship between demographic-reproduction reading and academic-freedom reading: foreclosure or coexistence?').

omega_variable(
    suppression_internalization_boundary,
    'Is the measured suppression primarily structural (external gatekeeping barriers) or primarily internalized (underrepresented scholars self-excluding due to identity fusion)?',
    'Post-exit trajectory analysis: track underrepresented scholars who depart academia and measure whether suppression persists (they carry internalized barriers to other fields) or resolves (they report liberation from gatekeeping constraints). If suppression persists in new contexts, it is substantially internalized. If suppression resolves, the structural mechanisms were primary.',
    'If suppression is substantially internalized, the exit-option classification (identity_locked) is confirmed, and the constraint''s persistence relies on cognitive capture as much as institutional mechanism—higher effective extraction. If suppression is primarily structural, exit becomes more viable once external barriers change (e.g., alternative career paths outside academia), and the constraint''s persistence depends on maintaining institutional gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_boundary, empirical, 'Structural vs. internalized mechanisms in the measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(tenu_tr_t5, tenure_contract__demographic_reproduction_reading, theater_ratio, 5, 0.43).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__demographic_reproduction_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(tenu_tr_t15, tenure_contract__demographic_reproduction_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__demographic_reproduction_reading, theater_ratio, 20, 0.59).
narrative_ontology:measurement(tenu_tr_t25, tenure_contract__demographic_reproduction_reading, theater_ratio, 25, 0.61).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__demographic_reproduction_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement(tenu_tr_t35, tenure_contract__demographic_reproduction_reading, theater_ratio, 35, 0.62).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__demographic_reproduction_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(tenu_be_t5, tenure_contract__demographic_reproduction_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__demographic_reproduction_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(tenu_be_t15, tenure_contract__demographic_reproduction_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__demographic_reproduction_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(tenu_be_t25, tenure_contract__demographic_reproduction_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__demographic_reproduction_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(tenu_be_t35, tenure_contract__demographic_reproduction_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__demographic_reproduction_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(tenu_su_t5, tenure_contract__demographic_reproduction_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__demographic_reproduction_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(tenu_su_t15, tenure_contract__demographic_reproduction_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__demographic_reproduction_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(tenu_su_t25, tenure_contract__demographic_reproduction_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__demographic_reproduction_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(tenu_su_t35, tenure_contract__demographic_reproduction_reading, suppression_requirement, 35, 0.81).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__demographic_reproduction_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_pipeline_demographic_attrition).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, contingent_labor_precarity).

% DUAL FORMULATION NOTE:
% The tenure_contract kernel admits three structurally distinct constraint stories: academic_freedom_reading (Mountains scholarly inquiry from political/institutional retaliation, high value for vulnerable researchers), demographic_reproduction_reading (this constraint: uses same mechanism to gatekeep demographic composition, high extraction from underrepresented groups), and institutional_extraction_reading (uses tenure to create employment rigidity favoring early winners, loads costs onto contingent labor). All three cite the same institutional structure and peer-review gatekeeping mechanism. They differ in which function is salient and which structural outcome they foreground. The three readings are related via network.affects_constraints (each influences the others' operating conditions) and via shared kernel_id. The demographic-reproduction reading coexists_with the academic-freedom reading (different parties hold both; a department can protect risky inquiry while also gatekeeping demographic composition) but influences it: widespread gatekeeping using 'fit' criteria undermines inquiry protection by making scholars fear innovative approaches will be marked as 'not serious' or 'poor fit.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__demographic_reproduction_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
