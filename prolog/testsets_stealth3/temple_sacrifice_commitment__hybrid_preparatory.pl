% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Suspended-Commitment Study Regime (Preparatory Reading)
 *   domain: religious_law/commitment_system_theory
 *
 * SUMMARY:
 *   A halakhic society carries a covenantal commitment to a Temple service
 *   that has been impossible to perform for roughly two millennia. Rather
 *   than letting the sacrificial corpus lapse, the tradition assigns it
 *   continuous, institutionalized study: academies train students in the
 *   anatomy of offerings, disqualifying blemishes, priestly procedure, and
 *   altar geometry — law with no scheduled occasion. This file authors the
 *   hybrid_preparatory reading of that arrangement: study maintains the
 *   commitment genuinely but in suspension — neither substitute-performance
 *   nor dead archive — as training whose justification is eventual messianic
 *   restoration. The interval maps the modern mass-yeshiva era (unit = one
 *   year, roughly mid-1960s to mid-2020s). The referent of epsilon is the
 *   standing study regime as it operates, assessed by this reading's own
 *   lights — never the restoration it anticipates. Claim and metrics are
 *   independent authored facts: the type is claimed from structural judgment
 *   (a genuine preparatory-coordination function coexisting with an
 *   asymmetric resource transfer, actively defended against defection and
 *   against rival framings), while the metrics describe observed operation,
 *   including the multiplication of the funded study population and the
 *   hardening of full-time-study norms over the interval.
 *
 * KEY AGENTS:
 *   - - rabbinic_scholarly_class: agenda-setter and principal beneficiary (institutional/identity_locked) — administers curriculum, ordination, and endowed positions; authority and livelihood fused with the corpus's centrality
 *   - - yeshiva_institutions: institutional beneficiary (institutional/identity_locked) — houses the program, receives tuition, donations, and subsidies, converts them into endowment and expansion
 *   - - yeshiva_students: dual-positioned payer-beneficiary (moderate/identity_locked) — supplies prime-age labor; collects stipends, standing, and identity; forgoes earnings and outside credentials
 *   - - funding_communities: payer (powerful/constrained) — diaspora donors and taxpayers financing mastery of law with no scheduled use
 *   - - women_barred_from_advanced_study: excluded payer (organized/trapped) — absorbs the household and wage-earning load that enables others' full-time study while denied the track that confers standing
 *   - - temple_restoration_activists: excluded challenger (organized/constrained) — insists suspended study is readiness in name only and presses for material preparation now
 *   - - academic_religion_scholars: analytical observer (analytical/analytical) — documents how communities carry commitments through periods when enactment is impossible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.46).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.47).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.46).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Suspended-Commitment Study Regime (Preparatory Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, '954739bf-6f65-4177-b14b-b1b8cfffd486').
narrative_ontology:cs_kernel_codification('954739bf-6f65-4177-b14b-b1b8cfffd486', fixed_text).
narrative_ontology:cs_authority_grounding('954739bf-6f65-4177-b14b-b1b8cfffd486', lineage).
narrative_ontology:cs_interpretation_layer_present('954739bf-6f65-4177-b14b-b1b8cfffd486').
narrative_ontology:cs_reading_relation('954739bf-6f65-4177-b14b-b1b8cfffd486', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('954739bf-6f65-4177-b14b-b1b8cfffd486', temple_sacrifice_commitment__performance_only, influences).
narrative_ontology:cs_axiom('954739bf-6f65-4177-b14b-b1b8cfffd486', foundational, study_maintains_suspended_commitment).
narrative_ontology:cs_axiom_status(study_maintains_suspended_commitment, holdable).
narrative_ontology:cs_axiom_grounding('954739bf-6f65-4177-b14b-b1b8cfffd486', study_maintains_suspended_commitment, instrumental).
narrative_ontology:cs_axiom('954739bf-6f65-4177-b14b-b1b8cfffd486', foundational, restoration_expectancy_keeps_suspension_temporary).
narrative_ontology:cs_axiom_status(restoration_expectancy_keeps_suspension_temporary, holdable).
narrative_ontology:cs_axiom_grounding('954739bf-6f65-4177-b14b-b1b8cfffd486', restoration_expectancy_keeps_suspension_temporary, theological).
narrative_ontology:cs_axiom('954739bf-6f65-4177-b14b-b1b8cfffd486', secondary, study_duty_survives_performance_impossibility).
narrative_ontology:cs_axiom_status(study_duty_survives_performance_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('954739bf-6f65-4177-b14b-b1b8cfffd486', study_duty_survives_performance_impossibility, conventional).
narrative_ontology:cs_reference_frame('954739bf-6f65-4177-b14b-b1b8cfffd486', covenant_service_suspended_pending_restoration).
narrative_ontology:cs_drift_state('954739bf-6f65-4177-b14b-b1b8cfffd486', contemporary_mass_yeshiva_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('954739bf-6f65-4177-b14b-b1b8cfffd486', '2026-08-01T12:00:00Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholarly_class).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, funding_communities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_students).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, women_barred_from_advanced_study).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_students).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, halakhic_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordains, sets curriculum, and allocates endowed study positions across the academy network. Decades of formation in the sacrificial corpus precede and constitute their authority; their teaching posts, matchmaking standing, and communal voice depend on the corpus remaining central. Stepping out of the study regime would mean forfeiting the authority structure their standing is built on.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholarly_class, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_scholarly_class, beneficiary).

% Academies and kollels that house the study program, receive tuition, donations, and state subsidies, and compete for enrollment and endowments. Their charters, buildings, and staff exist to sustain the study schedule; their continuity across generations is bound up with the program's continuation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Young men who commit their late teens and twenties to full-time study of texts including long tractates on offerings they cannot bring. They receive stipends, housing support, communal esteem, and arranged-marriage standing; they forgo vocational training, earnings, and outside credentials during the same years. Leaving mid-course carries family rupture and community censure.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_students, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_students, beneficiary).

% Diaspora philanthropists, federated charities, and taxpayers whose money underwrites stipends, buildings, and salaries. Most give out of loyalty to the study ideal; some question why substantial sums sustain mastery of rites with no scheduled occasion. A donor can redirect gifts at social cost; taxpayers cannot earmark away their share.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, funding_communities, payer,
    powerful, generational, constrained, global).

% Constituency historically denied access to the advanced track that confers standing in the study world, while absorbing the household and wage-earning load that makes others' full-time study possible. Would claim curriculum voice and teaching roles if admitted to the conversation that sets them.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, women_barred_from_advanced_study, excluded,
    organized, generational, trapped, global).

% Groups preparing priestly lineages, vessels, and site logistics for resumed offering. They argue that study alone leaves readiness theoretical and press for material steps now; the study establishment treats their agenda as premature, and their demands sit outside the forums where curricular weight is decided.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, temple_restoration_activists, excluded,
    organized, biographical, constrained, global).

% Historians and theorists of religion who document how communities carry commitments through periods when their enactment is impossible. They publish comparisons, attend conferences, and hold no stake in whether the corpus stays central; they describe the arrangement from outside it.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, academic_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__hybrid_preparatory, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves executable command of a large legal corpus across an open-ended period in which its rites cannot be performed, so that communal competence, textual accuracy, and institutional memory survive intact; organizes the allocation of young men's time, communal charity, and scholarly careers around that preservation task.
% TRANSFER_FUNCTION: Moves years of prime-age time and attention from students, household labor and wage-earning from their families, and donations plus tax subsidies from supporters and the public into a network of academies that converts these into preserved legal knowledge, credentialed teachers, and institutional permanence.
% ABSENT_VOICES: Women barred from the advanced track bear costs without a seat; restoration activists would insist suspended study is inadequate preparation and demand material programs now; secular members of the funding publics have no vote on subsidy levels. Each would contest what the study is for and who pays for it.
% DISAPPEARANCE_RATIONALE: If the study regime ended overnight, no rite resumes or ceases — nothing material happens at the altar, since none exists. But the scholarly class loses its charter discipline, the academies lose their distinctive subject matter, and the restoration-readiness claim loses its substance. Defenders predict rapid reconstitution from printed sources and recorded expertise; critics predict permanent demotion of the corpus from the curriculum and the communal imagination. Whether the world rearranges depends on which account of the study's function is true — hence the parties dispute.
% FOUNDING_PROBLEM: After the destruction of the Temple made the sacrificial order impossible to perform, the tradition faced loss-by-atrophy of a covenantal core: rites unpracticed fade, procedures are misremembered, texts are misread. The arrangement was built to keep the order recoverable until conditions for performance return.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic rabbinics attests, from documentary evidence of deliberate post-destruction preservation projects (early redaction, translation, codification), that the founding problem was real; restoration activists, who dispute the study-only remedy, nonetheless confirm the corpus remains non-performable today; state audit records and philanthropy research attest the ongoing transfer of public and charitable funds into study of the corpus. The genealogy is not attested by beneficiaries alone.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).
:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.46: the regime transfers substantial prime-age time and public/charitable money into mastery of non-performable law; the transfer is real but participants largely consent within their framework and receive identity goods alongside it, so it sits mid-range — and rises across the interval as the funded population multiplies and state subsidy institutionalizes. Suppression 0.47, authored UNSCALED: it is a raw structural property (communal censure of defection, family-formation timing, gatekept credentials), and only extractiveness gets scaled by directionality and scope in the engine's computation. The suppression_requirement series is included because this story specifically tracks enforcement-capacity change: full-time-study norms consolidated and censure machinery hardened over the interval, rather than suppression merely shadowing extraction. Theater_ratio 0.28: most study yields real textual competence, but a growing share functions as credential display and institutional legitimation detached from restoration-relevant skill. Accessibility_collapse 0.55: alternatives (trades, academia, other textual emphases) remain visible but close steeply once family formation and sunk years bind. Resistance 0.40: quiet internal questioning, feminist challenge, secular rejection of the framing, and restoration-activist pressure — enough friction that enforcement must keep working, which is why the suppression series climbs in step. All three series run on one shared grid (points 0–60 by tens) so the engine samples every metric at every authored time; the series are monotonic, with no cyclical dynamics modeled — the crisis-and-rebuild oscillation of earlier eras predates this interval. Identity-lock differs by seat: professional-institutional fusion for the scholarly class (the academy has become its function), ideological-relational fusion for students (selfhood constituted through the study bond, with marriage timed to it). Coalition note: the payer seats are heterogeneous — donors redirectable at social cost, taxpayers diffuse, students identity-bound, women gatekept — which has so far blocked a combined redirection of resources; a funder-women-student coalition is the live threat the enforcement apparatus implicitly manages. Receipt surface: the material flows (tuition, gifts, subsidies) demonstrably accrue to yeshiva_institutions, which convert them into endowment, staffing, and expansion; scholarly prestige rides on that institutional base, so gain_flow names the institutional seat. Fixing cost: the agenda-setters could rebalance curricula at will, but doing so dissolves the identity and authority structure they ARE — prohibitive relative to the benefit of honest realignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting scholar seat and the payer seats should compute differently. To the scholar, decades inside the corpus are vocation, identity, and the source of every standing he holds; the arrangement presents as continuity itself, and his fused identity makes demotion of the corpus unthinkable rather than merely costly. To funding communities, the same arrangement presents as open-ended subsidy of mastery with no scheduled use — salient precisely where the money is compulsory. Students sit between: the identity rewards are immediate and the forgone earnings compound slowly, and identity-lock binds through family formation timed to the study years. The excluded seats sharpen the divergence: women absorb the enabling household economics without access to the standing-conferring track, and restoration activists see preparation without a material object as readiness in name only. Same-level differentiation: donors and taxpayers share nominal power yet differ sharply in exit (gifts redirectable at social cost, taxes not earmarkable away), which the engine reads from exit_options rather than labels. The engine computes per-seat classifications from this structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map directly onto directional pull. rabbinic_scholarly_class and yeshiva_institutions sit at the beneficiary pole (d near 0): the regime subsidizes their status, income, and perpetuation, and their exits are identity-fused. funding_communities sit near the target pole (d near 1): they supply the transfer under constrained exit. women_barred_from_advanced_study bear cost without access — effectively a full target. temple_restoration_activists are excluded rather than coordinated; their position registers through absence and through the network edge to the performance-oriented reading, not through d. academic_religion_scholars are analytical. One override is declared: for yeshiva_students (the sole stakeholder at moderate power). A derivation reading the victim list alone would push the student seat toward full target, but students also collect stipends, esteem, marriage standing, and a promised place in the restored order — a genuine beneficiary component layered on real opportunity cost. The override sets d=0.62: majority-target with a non-trivial beneficiary share. Suppression, again, is authored unscaled; the engine scales only extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is keeping two errors apart. Calling the arrangement pure extraction would erase the genuine preparatory function this reading asserts — the corpus really is kept recoverable, and the competence claim is not empty cover. Romanticizing it as pure coordination would erase the asymmetric transfer, the concentrated institutional capture, and the absence of any declared sunset: the tradition's own refusal to compel the end means the transitional justification carries no termination mechanism, two millennia of 'temporary' suspension notwithstanding — which is why has_sunset_clause is authored false and no transitional-support type is claimed. Mandatrophy is currently unresolved: the founding problem (a non-performable corpus needing preservation) is live, so the arrangement's mandate has not outlived its function. The failure mode to watch is drift toward inertial maintenance: if restoration expectancy thins into rhetoric while curricular weight persists, the performative share climbs and the preparatory claim becomes performance — the cs_structure drift_state records that practice-drift is already substantial and unacknowledged. The R5 mismatch consumer reads status=live x verdict=contested: no zombie flag fires, correctly, because the parties dispute the function's reality, not the fact that the arrangement persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_status_of_suspended_study,
    'This constraint is one reading (hybrid_preparatory) of the kernel temple_sacrifice_commitment; the sibling readings study_as_exercise and performance_only assign suspended study opposite statuses (complete occupation now / inert archive of a defunct practice). Which status does engagement with non-performable sacrificial law actually confer on the commitment?',
    'Comparative-historical test: examine episodes where restoration conditions approached or were credibly simulated (modern revival projects, historical interruptions of performance) and measure whether preparatory study converted into performable readiness at the rate the preparatory claim requires; triangulate with doctrinal adjudication across the living readings.',
    'If the archive verdict holds, the regime loses its coordination function and drifts toward inertial, theater-dominated persistence; if the occupation verdict holds, study is fulfillment rather than preparation and the extraction reading inverts; only the preparatory verdict sustains the classification claimed here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_status_of_suspended_study, conceptual, 'Sibling-reading contest over what suspended study does to the commitment.').

omega_variable(
    restoration_expectancy_actionability,
    'Is the restoration expectancy that justifies preparatory investment held as an actionable planning assumption or as open-ended hope?',
    'Behavioral audit: budget allocations to restoration-relevant skills versus general curriculum, implied horizons in official pronouncements, and demographic turnover in restoration-focused subfields.',
    'If open-ended, the preparatory justification weakens, the performative share rises, and the regime slides toward inertia; if actionable, part of the measured extraction is repaid by genuine readiness value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_expectancy_actionability, empirical, 'Whether the restoration premise backing the preparatory logic is operative or rhetorical.').

omega_variable(
    student_net_position_heterogeneity,
    'Do the goods students receive (stipends, communal standing, marriage-market position, promised restoration-era roles) outweigh their forgone earnings and credentials on net, and uniformly across the student body?',
    'Longitudinal tracing of economic and welfare outcomes for leavers versus stayers, stratified by aptitude and family resources.',
    'If benefits dominate for most students, the student seat flips beneficiary-side and the extraction burden concentrates on funders and the excluded constituency; if costs dominate, the student seat''s target-weighting stands and its effective burden is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(student_net_position_heterogeneity, empirical, 'Net-cost heterogeneity inside the dual-positioned student seat.').

omega_variable(
    exclusion_suppression_mechanism,
    'Is the exclusion of women from the advanced study track maintained by structural barriers (gatekept admissions, institutional rules) or by internalized role acceptance, or both?',
    'Post-opening uptake trajectories in communities that have admitted women to advanced text study: a surge upon opening indicates structural suppression; flat uptake indicates internalization.',
    'If largely structural, lifting barriers redistributes cost-bearing quickly and the regime''s coercive surface shrinks; if internalized, suppression persists after formal barriers fall and runs deeper than admission rules show.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_suppression_mechanism, empirical, 'Structural versus internalized mechanism behind the exclusion''s persistence.').

omega_variable(
    undeclared_sunset_character,
    'This reading describes itself as preparatory — transitional by definition — yet declares no termination mechanism and the tradition declines to compel the end. Is restoration a genuine terminating condition for the arrangement, or an indefinitely renewable deferral that lets a steady-state regime wear transitional clothing?',
    'Doctrinal and behavioral test: whether any authority conditions curricular weight or resource claims on proximity-to-restoration criteria, and whether the regime''s scale tracks restoration probability at all.',
    'A genuine terminating condition would support transitional-support dynamics with the justification residing in the transition itself; renewable deferral confirms steady-state operation and makes the missing sunset the load-bearing fact behind the hybrid classification claimed here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(undeclared_sunset_character, conceptual, 'Undeclared sunset: genuine termination criterion versus indefinitely renewable deferral.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsac_hybrid_prep_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(tsac_hybrid_prep_tr_t0, observed).
narrative_ontology:measurement(tsac_hybrid_prep_tr_t10, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(tsac_hybrid_prep_tr_t10, observed).
narrative_ontology:measurement(tsac_hybrid_prep_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(tsac_hybrid_prep_tr_t20, observed).
narrative_ontology:measurement(tsac_hybrid_prep_tr_t30, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(tsac_hybrid_prep_tr_t30, observed).
narrative_ontology:measurement(tsac_hybrid_prep_tr_t40, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(tsac_hybrid_prep_tr_t40, observed).
narrative_ontology:measurement(tsac_hybrid_prep_tr_t50, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 50, 0.26).
narrative_ontology:measurement_basis(tsac_hybrid_prep_tr_t50, observed).
narrative_ontology:measurement(tsac_hybrid_prep_tr_t60, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(tsac_hybrid_prep_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(tsac_hybrid_prep_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(tsac_hybrid_prep_be_t0, observed).
narrative_ontology:measurement(tsac_hybrid_prep_be_t10, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(tsac_hybrid_prep_be_t10, observed).
narrative_ontology:measurement(tsac_hybrid_prep_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.34).
narrative_ontology:measurement_basis(tsac_hybrid_prep_be_t20, observed).
narrative_ontology:measurement(tsac_hybrid_prep_be_t30, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(tsac_hybrid_prep_be_t30, observed).
narrative_ontology:measurement(tsac_hybrid_prep_be_t40, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 40, 0.41).
narrative_ontology:measurement_basis(tsac_hybrid_prep_be_t40, observed).
narrative_ontology:measurement(tsac_hybrid_prep_be_t50, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 50, 0.44).
narrative_ontology:measurement_basis(tsac_hybrid_prep_be_t50, observed).
narrative_ontology:measurement(tsac_hybrid_prep_be_t60, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 60, 0.46).
narrative_ontology:measurement_basis(tsac_hybrid_prep_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(tsac_hybrid_prep_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(tsac_hybrid_prep_su_t0, observed).
narrative_ontology:measurement(tsac_hybrid_prep_su_t10, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(tsac_hybrid_prep_su_t10, observed).
narrative_ontology:measurement(tsac_hybrid_prep_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(tsac_hybrid_prep_su_t20, observed).
narrative_ontology:measurement(tsac_hybrid_prep_su_t30, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(tsac_hybrid_prep_su_t30, observed).
narrative_ontology:measurement(tsac_hybrid_prep_su_t40, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 40, 0.43).
narrative_ontology:measurement_basis(tsac_hybrid_prep_su_t40, observed).
narrative_ontology:measurement(tsac_hybrid_prep_su_t50, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 50, 0.45).
narrative_ontology:measurement_basis(tsac_hybrid_prep_su_t50, observed).
narrative_ontology:measurement(tsac_hybrid_prep_su_t60, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 60, 0.47).
narrative_ontology:measurement_basis(tsac_hybrid_prep_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% The colloquial label 'study of the sacrificial laws' covers four structurally distinct arrangements with different epsilons: study-as-exercise (engagement as fulfillment — the cost of study reads as the cost of performing a command), hybrid-preparatory (this file — suspended maintenance, moderate extraction against an uncertain restoration), performance-only (study as archive — little live extraction, persistence mostly inertial), and symbolic-transformation (authorized substitution — extraction rides the new instantiation of prayer and study). Authored separately per the epsilon-invariance principle; this file's epsilon refers only to the preparatory reading. Family links preserve the dependency structure: the preparatory corpus is cited as feasibility evidence by performance-oriented revivalists, and the exercise reading shares this reading's institutional substrate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__hybrid_preparatory, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
