% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__demographic_reproduction_reading, []).

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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   Tenure peer review in contemporary U.S. higher education operates under
 *   an ostensible mandate to evaluate research excellence, but empirical
 *   evidence documents systematic disparities in tenure access and denial by
 *   demographics that correlate poorly with research productivity metrics and
 *   strongly with demographic similarity of evaluators to candidates. The
 *   constraint story describes the demographic-reproduction reading: tenure's
 *   peer-review mechanism functions as gatekeeping that reproduces the
 *   demographic composition of academia's elite through evaluation criteria
 *   ('fit,' 'collegiality') that are operationally decoupled from research
 *   quality and codependent with demographic homophily. This reading asserts
 *   that tenure protects demographic closure against diversification pressure
 *   more effectively than it protects inquiry against external censorship.
 *   The claim is Tangled Rope: a real coordination function (peer evaluation
 *   of research quality, protection of inquiry from political pressure)
 *   paired with asymmetric extraction (research autonomy and job security
 *   concentrated among majority-group faculty; barriers to tenure-track entry
 *   and tenure security concentrated among minority-group researchers).
 *   Active enforcement occurs through committee gatekeeping, gatekeeping-norm
 *   transmission via graduate training, and administrative risk management
 *   oriented toward protecting tenure denials against discrimination
 *   litigation. Suppression is substantial (0.72): minority-group researchers
 *   face documented barriers to entry and advancement; contingent labor
 *   (disproportionately staffed by excluded groups) is structurally trapped.
 *   Theater ratio is elevated (0.58 rising to 0.60): performance of
 *   'objective merit evaluation' increases as the actual gatekeeping
 *   mechanism's demographic nature becomes more visible and contested;
 *   institutions perform evaluation rigor (external reviews, documentation,
 *   training) while maintaining gatekeeping outcomes through more subtle
 *   operational moves ('fit' judgments, committee composition, evaluation
 *   criteria weighting).
 *
 * KEY AGENTS:
 *   - Demographically dominant faculty (institutional power; agenda-setter; beneficiary of gatekeeping and tenure protection; arbitrage exit): set evaluation criteria, control review committees, collect research autonomy and job security, protected by tenure from accountability for evaluation decisions
 *   - Underrepresented demographic groups (moderate power; payer; face gatekeeping barriers; constrained exit): navigate subjective evaluation through proxies for demographic homophily; contingent positions or tenure denials; documented disparities in productivity metrics vs. outcomes
 *   - Early-career researchers from minority backgrounds (powerless; payer; identity-locked; biographical time horizon): career path dependent; identity formation during probationary period; face institutional evaluation through homophily proxies; unable to challenge criteria without career jeopardy
 *   - Contingent academic labor (powerless; payer; trapped; immediate time horizon): bear the cost of tenure lock-in; teaching loads that cross-subsidize majority-group research time; majority-group faculty research autonomy funded by contingent precarity; disproportionately from excluded demographics
 *   - Research funders (institutional power; observer; analytical exit): measure research output independent of tenure status; document no correlation between research impact and tenure access; possess external leverage through resource dependency
 *   - Professional associations (organized power; excluded; constrained exit): document that 'collegiality' and 'fit' correlate with demographic similarity, not research quality; excluded from tenure decisions; their standards are displaced by internal faculty consensus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.68).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.72).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.69).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, '7067f409-81dc-4070-a2e0-7fd8204ac80f').
narrative_ontology:cs_kernel_codification('7067f409-81dc-4070-a2e0-7fd8204ac80f', fixed_text).
narrative_ontology:cs_authority_grounding('7067f409-81dc-4070-a2e0-7fd8204ac80f', extraction).
narrative_ontology:cs_interpretation_layer_present('7067f409-81dc-4070-a2e0-7fd8204ac80f').
narrative_ontology:cs_reading_relation('7067f409-81dc-4070-a2e0-7fd8204ac80f', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('7067f409-81dc-4070-a2e0-7fd8204ac80f', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('7067f409-81dc-4070-a2e0-7fd8204ac80f', foundational, demographic_homophily_operationalizes_fit).
narrative_ontology:cs_axiom_status(demographic_homophily_operationalizes_fit, holdable).
narrative_ontology:cs_axiom_grounding('7067f409-81dc-4070-a2e0-7fd8204ac80f', demographic_homophily_operationalizes_fit, empirically_contingent).
narrative_ontology:cs_axiom('7067f409-81dc-4070-a2e0-7fd8204ac80f', secondary, gatekeeping_authority_requires_demographic_closure).
narrative_ontology:cs_axiom_status(gatekeeping_authority_requires_demographic_closure, holdable).
narrative_ontology:cs_axiom_grounding('7067f409-81dc-4070-a2e0-7fd8204ac80f', gatekeeping_authority_requires_demographic_closure, instrumental).
narrative_ontology:cs_reference_frame('7067f409-81dc-4070-a2e0-7fd8204ac80f', peer_review_meritocratic_selection).
narrative_ontology:cs_drift_state('7067f409-81dc-4070-a2e0-7fd8204ac80f', contemporary_diversity_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7067f409-81dc-4070-a2e0-7fd8204ac80f', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_demographic_groups).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, early_career_researchers_from_minority_backgrounds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, contingent_academic_labor).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, institutional_self_preservation_doctrine).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, cultural_homophily_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets evaluation criteria nominally about research excellence but operationalized through 'fit' and 'collegiality' judgments that correlate strongly with demographic similarity. Controls hiring, tenure review, and promotion committees. Benefits from tenure protection that locks in their cohort's position and shields them from external accountability for evaluation decisions. Can arbitrage to other institutions at higher ranks; exit costs are low relative to their power.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, beneficiary).

% Face tenure evaluation through criteria ('collegiality,' 'departmental fit,' 'shared intellectual culture') that are documented to proxy for demographic homophily and are evaluated by committees demographically similar to the gatekeeping majority. Research productivity metrics are secondary to these subjective criteria. Hold contingent or non-tenure-track positions at high rates; tenure-track entry is constrained by demographic screening in hiring. Exit involves leaving academia or relocating to institutions with different evaluation cultures, both costly. Cannot easily contest evaluation decisions without jeopardizing future career prospects.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_demographic_groups, payer,
    moderate, biographical, constrained, national).

% Navigate a six-year probationary period where tenure decisions are made by committees whose demographic composition and evaluation criteria carry embedded homophily. Career path dependence (PhD debt, geographic constraints from partnerships, institutional identity as 'early-career researcher') locks them into the institutional review process. Their identity as researchers is being formed during the period they are being evaluated through proxies for demographic fit. Possess strong research records but face unexplained rejections or unfavorable comparisons to majority-group candidates with similar or lower productivity metrics. Inability to challenge criteria without risking non-renewal.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, early_career_researchers_from_minority_backgrounds, payer,
    powerless, biographical, identity_locked, national).

% Bears the structural cost of tenure lock-in: tenure protection for the majority creates institutional rigidity that loads teaching and service labor onto non-tenure-track, contingent positions as a cost-absorption mechanism. Majority-group faculty enjoy research time and research support funded by contingent labor's teaching load. Locked in by precarity: semester-by-semester contracts, lack of benefits, no research support, impossible to simultaneously teach three courses and maintain competitive research records. Demographic composition of contingent labor overrepresents groups excluded from tenure pathways.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, contingent_academic_labor, payer,
    powerless, immediate, trapped, national).

% Administer the tenure system operationally but do not set the evaluation criteria themselves — those are determined by disciplinary norms and majority-group faculty consensus. Responsible for defending tenure decisions against discrimination claims; motivation is institutional risk mitigation rather than evaluation accuracy. Perform the surveillance and documentation function that makes the gatekeeping appear rule-governed.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, department_administrators, agenda_setter,
    organized, generational, constrained, national).

% Measure research output and citation impact and find no statistical justification for the exclusion rates and evaluation disparities they observe in tenure outcomes. Possess external metrics (grant awards, publication impact) independent of internal departmental judgment. Can condition funding on diversity metrics but cannot directly alter tenure evaluation criteria. Hold analytical distance but have leverage through resource dependency.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, research_funders, observer,
    institutional, generational, analytical, global).

% Issue guidelines distinguishing research evaluation from demographic factors; document that 'collegiality' and 'fit' correlate with demographic similarity and lack empirical connection to research quality or institutional function. Are excluded from tenure-review decisions themselves; their objections are treated as external critique rather than binding authority. Their role would be to establish evaluation standards; that role is displaced into internal faculty consensus.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, professional_associations, excluded,
    organized, generational, constrained, national).

% Document disparate impact in tenure outcomes by demographic category; raise discrimination flags in internal review. Are structurally excluded from final tenure decisions — their data enters as commentary, not as a gate. Department faculty retain final authority. Constrained by institutional hierarchy: challenging majority-group faculty directly risks being marginalized.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, diversity_and_inclusion_offices, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Evaluates research quality through peer review by disciplinary experts with deep knowledge of the field; protects researchers from institutional retaliation or pressure to conform inquiry to majority preferences; stabilizes academic positions so researchers can pursue long-term, high-risk projects without concern for annual evaluation cycles.
% TRANSFER_FUNCTION: Transfers research autonomy, job security, and institutional protection to tenured faculty (concentrated among majority-group members); simultaneously restricts these transfers through gatekeeping that correlates with demographic similarity; loads teaching labor, service obligations, and precarity onto contingent academic labor (disproportionately from excluded demographics); extracts demographic compliance (conformity to majority-group norms, 'fit,' 'collegiality') as an implicit condition of permanent employment.
% ABSENT_VOICES: Researchers excluded from tenure pathways are structurally absent from peer-review committees that judge their peers, creating a systematic bias against their evaluation. Contingent faculty, who bear the cost of tenure lock-in through teaching-load cross-subsidy of majority-group research time, have no governance voice in tenure standards or resource allocation. Professional associations and external research funders possess independent metrics showing disparities in tenure access vs. research quality but are excluded from tenure decisions — their data enters as external critique, not as binding authority.
% DISAPPEARANCE_RATIONALE: If tenure peer review as demographic gatekeeping disappeared — whether through tenure abolition, shift to external evaluation, decoupling of 'fit' and 'collegiality' from tenure decisions, or composition-blind review — the demographic distribution of academia would shift substantially. Majority-group faculty's gatekeeping authority would diminish or be overseen. Contingent labor pools would shrink as resource allocation decoupled from gatekeeping. Department cultures would reorganize around different norms. Research resource allocation would track measured impact rather than evaluator preference. The constraint maintains a specific power and demographic distribution; its removal would rearrange it.
% FOUNDING_PROBLEM: Academic inquiry faces pressure from institutional, political, and commercial actors to censor, suppress, or penalize research findings that are inconvenient to power. Early academic professions lacked mechanisms to protect researchers from retaliation.
% FOUNDING_PROBLEM_CORROBORATION: External corroboration of the founding problem's persistence comes from: (1) Research funders' documented instances of political pressure on universities to restrict research on climate, gender, race, and economics; (2) Comparison institutions and international cases where tenure has been weakened and academic research has become more politically constrained; (3) Internal institutional pressure (documented through FOIA records and litigation discovery) to discipline or non-renew researchers whose findings are politically unpopular with administration. HOWEVER: The demographic-reproduction reading asserts that tenure's protection against this pressure is selective: it protects majority-group researchers whose work is politically unpopular with external actors (climate research skeptics, free-speech advocates) while it fails to protect minority-group researchers whose work is mainstream (standard economics, DEI research) but unpopular with gatekeeping faculty. External research funders and professional associations attest that tenure gatekeeping does NOT correlate with research impact or protection of political unpopularity; it correlates with demographic similarity of evaluators to candidates. This corroboration supports the reading that tenure gatekeeping operates independently of its stated founding problem.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at end of interval) because evaluation criteria are operationally decoupled from research productivity (the primary justified function) and coupled with demographic similarity (an unjustified criterion). Measurement series show rising extractiveness over the interval: as external pressure to diversify increases, gatekeeping mechanisms become more sophisticated and supple, maintaining demographic closure through more subtle operational moves ('fit' judgments weighted more heavily, committee composition managed, evaluation criteria applied with demographic variance). Theater ratio rises steeply (0.35 to 0.60) because performance of 'objective merit evaluation' increases as the gatekeeping function becomes more visible: external reviews, documentation protocols, diversity training, and evaluation rubrics proliferate while gatekeeping outcomes persist. Suppression is high (0.72) because barriers are both structural (gatekeeping committees, non-renewal, discrimination) and internalized (self-selection, research agenda modification to fit majority preferences, learned exit). Accessibility of alternatives is limited (0.69 collapse) for minority researchers: alternative career paths exist but tenure-track pathways are constrained by gatekeeping; leaving academia is an option but identity formation during the probationary period and career path dependence create high exit costs. Resistance to the constraint is moderate (0.55) because it operates with institutional legitimacy (peer review, meritocracy framing) and because resistance from excluded parties is individually costly (career jeopardy) and collectively difficult to coordinate (dispersed locations, precarious positions, no institutional voice). The measurement series captures the dynamic: extractiveness accelerates in the first two decades as external diversity pressure increases and gatekeeping becomes more sophisticated; theater ratio rises faster than extractiveness as the performance of 'objective evaluation' is deployed to manage legitimacy challenges; suppression requirement plateaus (0.72–0.75) because the gatekeeping mechanism is now well-embedded and internalized, requiring less active enforcement to maintain.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (demographically dominant faculty, institutional power) experiences the constraint as coordination: peer review by equals ensures research quality, provides autonomy and job security, and protects their research from political pressure. They see gatekeeping as quality control and cohort stability. The payer seats (underrepresented groups, contingent labor) experience the same constraint as extraction: evaluation through criteria that correlate with demographic similarity, systematic denial of tenure access and security, research autonomy rationed by gatekeeping committees, and job precarity. From the payer seats, gatekeeping is experienced as demographic closure and institutional exclusion. The engine's per-seat classification will diverge because the structural data — power level, exit options, beneficiary vs. victim declaration — differ across seats. Demographically dominant faculty sit near d=0.2 (near beneficiary end): they hold institutional power, have arbitrage exit, declared as beneficiary. Underrepresented groups sit near d=0.85 (near target end): they have moderate to powerless positions, constrained to identity-locked exit, declared as victims. The divergence is not a measurement error — it is the structural asymmetry the reading asserts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality encodes the structural relationship to THIS constraint. Demographically dominant faculty: declared as beneficiary (collect tenure security, research autonomy, gatekeeping authority); hold institutional power (can set evaluation criteria and committee composition); possess arbitrage exit (can move to other institutions at higher ranks with tenure protection secured). Derivation chain yields d near 0.2 (low, favoring beneficiary end): beneficiary status + high power + arbitrage-grade exit = low directionality, implying low effective extraction for this seat (they benefit from the constraint, so extraction is inverted into subsidy or is near zero). Underrepresented groups: declared as victim (face gatekeeping barriers, tenure denial disparities, structural constraints); hold moderate power (some institutional presence, some ability to organize and document disparities, but not decision-making authority); exit options range from constrained (for those in tenure-track positions) to identity-locked (for early-career researchers where career and identity are being formed during probationary evaluation) to trapped (for contingent labor). Derivation chain yields d near 0.80–0.90 (high, targeting end): victim status + moderate-to-powerless position + constrained-to-trapped exit = high directionality, implying high effective extraction for this seat (they bear the constraint's costs and cannot exit easily, so extraction is amplified). Contingent labor: declared as payer (bear teaching loads that subsidize majority-group research); hold powerless position (no hiring authority, no governance voice); trapped exit (precarious contracts, no path to tenure, geographic constraints). Derivation chain yields d near 0.95 (full target): payer status + powerless position + trapped exit = maximum directionality, implying maximum effective extraction (bears cost, cannot resist, cannot exit). No directionality overrides are authored; the derivation chain captures the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Tenure's founding problem was to stabilize inquiry against external political pressure. The demographic-reproduction reading asserts that the founding problem persists live (external pressure to censor research remains real) but is substantially separated from the gatekeeping mechanism: tenure gatekeeping operates to control demographic composition of academia, not to protect politically unpopular research; externally unpopular research may be protected or suppressed depending on whether the researcher's demographic identity aligns with the gatekeeping majority. The constraint shows mandatrophy signals: (1) Theater ratio rising (performance of 'objective evaluation' increasing as gatekeeping's demographic operation becomes more visible); (2) Founding problem status: contested (academics claim tenure protects inquiry; external observers claim tenure protects demographic closure); (3) Disappearance verdict: world_rearranges (if demographic gatekeeping were removed, academia's composition would shift). The mandatrophy scenario is: tenure's stated mandate (protect academic freedom) and actual effect (protect demographic closure) have decoupled. The constraint is preserved not because the founding problem demands it but because the gatekeeping mechanism benefits the majority-group faculty who control tenure decisions. Active enforcement persists even as the founding problem's solution-set expands (external pressure to censor remains; multiple institutional mechanisms could address it; tenure gatekeeping is retained because it also protects demographic closure). This satisfies mandatrophy (mandate outlived by function preservation through extraction) under the demographic-reproduction reading. The academic-freedom reading contests this: it asserts tenure still protects inquiry and that gatekeeping is a perverse effect, not the central function. The institutional-extraction reading asserts tenure's founding problem is solved but the mechanism is preserved as rent extraction by early winners (the majority-group faculty now secure in tenure). All three readings agree that some function has shifted or ossified; they disagree about what the constraint's operative function now is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fit_collegiality_measurement_ambiguity,
    'Are ''fit'' and ''collegiality'' objective predictors of research quality and departmental function, or do they operationalize demographic preference under the cover of merit language?',
    'Regression analysis decoupling demographic similarity from research productivity and tenure outcomes; linguistic analysis of tenure files showing whether ''fit'' language appears preferentially in evaluations of majority-group vs. minority-group candidates with equivalent research records; controlled experiments where identical CVs are evaluated with and without demographic identifiers.',
    'If ''fit'' and ''collegiality'' are objective predictors, they are coordination requirements; the constraint is legitimate Tangled Rope (coordination cost extracted through demographic conformity). If they operationalize demographic preference uncorrelated with research quality, the constraint reclassifies toward Snare (pure extraction riding institutional authority).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fit_collegiality_measurement_ambiguity, empirical, 'Whether merit criteria proxy demographic preference or genuine organizational function.').

omega_variable(
    academic_freedom_contingency,
    'Does tenure protect inquiry against external pressure to censor research unrelated to institutional demographics, or does tenure primarily protect demographic closure against internal pressure to diversify?',
    'Comparative case analysis: tenure denials for politically controversial research vs. tenure denials for researchers from underrepresented groups; documented instances where tenure protected whistle-blowers vs. documented instances where tenure denied researchers alleged discrimination; international comparison of tenure vs. academic freedom outcomes.',
    'If tenure protects academic freedom for all researchers equally regardless of demographic group, the demographic-reproduction reading''s gatekeeping claim is overstated. If tenure is disproportionately withheld from minority-group researchers, academic freedom protection is unevenly distributed, supporting Tangled Rope classification (coordination + asymmetric extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(academic_freedom_contingency, empirical, 'Whether tenure equalizes or stratifies academic freedom protection.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (enforced through external barriers: gatekeeping committees, non-renewal, discrimination) or internalized (minority researchers self-selecting out, modifying research agendas)?',
    'Prospective tracking of PhD recipients'' research interests vs. career directions by demographic group; exit-rate analysis comparing majority vs. minority researchers at tenure-relevant periods; post-exit interviews assessing preference-driven vs. constraint-driven exit.',
    'If suppression is primarily structural, persistence depends on active enforcement and could shift with policy change. If primarily internalized, the constraint persists after formal barriers are removed. The distinction affects fixing requirements: structural suppression requires removing gatekeeping mechanisms; internalized suppression requires cultural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized self-exclusion.').

omega_variable(
    reading_relationship_to_academic_freedom_reading,
    'Does the demographic-reproduction reading''s core claim about tenure gatekeeping foreclose the academic-freedom reading''s claim that tenure protects inquiry, or can both readings coherently describe the same tenure system?',
    'Examine evidence compatibility: if tenure protects inquiry for some researchers (majority-group members with politically unpopular views) while gatekeeping others (minority-group members), both readings coexist. If tenure uniformly protects inquiry regardless of demographics, academic-freedom reading is correct and demographic-reproduction reading overstates gatekeeping.',
    'Resolution determines reading_relations classification: conditional application = coexist_with; logically incompatible claims = forecloses. Current authoring assumes coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relationship_to_academic_freedom_reading, conceptual, 'Relationship between demographic-reproduction reading and academic-freedom reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(tenu_tr_t0, observed).
narrative_ontology:measurement(tenu_tr_t5, tenure_contract__demographic_reproduction_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(tenu_tr_t5, observed).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__demographic_reproduction_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(tenu_tr_t10, observed).
narrative_ontology:measurement(tenu_tr_t15, tenure_contract__demographic_reproduction_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(tenu_tr_t15, observed).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__demographic_reproduction_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement_basis(tenu_tr_t20, observed).
narrative_ontology:measurement(tenu_tr_t25, tenure_contract__demographic_reproduction_reading, theater_ratio, 25, 0.59).
narrative_ontology:measurement_basis(tenu_tr_t25, observed).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__demographic_reproduction_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement_basis(tenu_tr_t30, observed).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__demographic_reproduction_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(tenu_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(tenu_be_t0, observed).
narrative_ontology:measurement(tenu_be_t5, tenure_contract__demographic_reproduction_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(tenu_be_t5, observed).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__demographic_reproduction_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(tenu_be_t10, observed).
narrative_ontology:measurement(tenu_be_t15, tenure_contract__demographic_reproduction_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(tenu_be_t15, observed).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__demographic_reproduction_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(tenu_be_t20, observed).
narrative_ontology:measurement(tenu_be_t25, tenure_contract__demographic_reproduction_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(tenu_be_t25, observed).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__demographic_reproduction_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(tenu_be_t30, observed).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__demographic_reproduction_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(tenu_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(tenu_su_t0, observed).
narrative_ontology:measurement(tenu_su_t5, tenure_contract__demographic_reproduction_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(tenu_su_t5, observed).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__demographic_reproduction_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(tenu_su_t10, observed).
narrative_ontology:measurement(tenu_su_t15, tenure_contract__demographic_reproduction_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(tenu_su_t15, observed).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__demographic_reproduction_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(tenu_su_t20, observed).
narrative_ontology:measurement(tenu_su_t25, tenure_contract__demographic_reproduction_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement_basis(tenu_su_t25, observed).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__demographic_reproduction_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement_basis(tenu_su_t30, observed).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(tenu_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__demographic_reproduction_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% Tenure operates as a constraint that is read differently by different institutional parties. The demographic_reproduction_reading asserts that peer-review gatekeeping functions primarily as demographic closure (benefiting majority-group faculty, extracting from underrepresented groups) rather than as academic freedom protection. This reading shares the kernel (commitment to presumptively permanent peer-reviewed employment) with academic_freedom_reading and institutional_extraction_reading, but assigns the constraint different operative functions and different beneficiary structures. The three readings have different ε values (high for demographic gatekeeping, variable for academic freedom, medium for institutional extraction) and different victim sets, reflecting distinct empirical claims about what tenure's primary effect is. Each reading is generated as a separate constraint JSON; they are linked via network.affects_constraints to model that they decompose the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
