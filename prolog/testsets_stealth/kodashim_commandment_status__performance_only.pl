% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Study Investment under the Performance-Only Reading (Altar-Contingent Husk)
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   When Rome destroyed the Second Temple in 70 CE, the largest legislative
 *   block in the Torah lost its performance site. The rabbinic commitment
 *   system responded by preserving the sacrificial corpus in full and
 *   building an educational order in which mastery of it remains central,
 *   credentialed, and prestigious nineteen centuries later. This story
 *   instantiates the performance_only reading of the
 *   kodashim_commandment_status kernel: on this reading the commandment is
 *   strictly altar-contingent, and with no altar it is a husk — suspended,
 *   with no residual normative force that study could discharge or readiness
 *   could serve. The standing arrangement under assessment is therefore the
 *   continued diversion of first-rank scholarly labor, curricular capacity,
 *   and communal funding into a legally inert body of material, with
 *   authority and prestige accruing to the institutions and specialists who
 *   administer it. The claim/metric independence rule applies: claimed_type
 *   records this reading's structural verdict; the metrics record descriptive
 *   operating facts; the engine computes per-seat classifications from the
 *   structural data, and divergence between claim and computed type is
 *   signal, not error.
 *
 * KEY AGENTS:
 *   - rabbinic_academy_establishment: agenda-setting administrator (institutional/identity_locked) — sets curriculum, ordains, collects authority
 *   - kodashim_scholarship_specialists: primary beneficiary (organized/identity_locked) — collect prestige keyed to the inert block
 *   - yeshiva_students: primary target (powerless/constrained) — bear diverted prime-year labor; coalition-capable in aggregate
 *   - aspiring_halakhic_decisors: secondary target (moderate/constrained) — training time diluted, credentials partly dependent
 *   - diaspora_lay_communities: dual-position funder (organized/constrained) — receive continuity goods, pay sustenance
 *   - temple_restoration_movements: excluded voice (organized/trapped) — contest the dormancy premise from outside the conversation
 *   - academic_jewish_studies_scholars: analytical observer (institutional/analytical) — document the arrangement's history and economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.74).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.62).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.74).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, snare).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Study Investment under the Performance-Only Reading (Altar-Contingent Husk)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic/commitment-system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, 'd9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c').
narrative_ontology:cs_kernel_codification('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', fixed_text).
narrative_ontology:cs_authority_grounding('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', lineage).
narrative_ontology:cs_interpretation_layer_present('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c').
narrative_ontology:cs_reading_relation('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', kodashim_commandment_status__messianic_deferral, forecloses).
narrative_ontology:cs_reading_relation('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_axiom('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', foundational, commandment_validity_exhausted_without_altar).
narrative_ontology:cs_axiom_status(commandment_validity_exhausted_without_altar, holdable).
narrative_ontology:cs_axiom_grounding('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', commandment_validity_exhausted_without_altar, conventional).
narrative_ontology:cs_axiom('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', secondary, suspended_law_study_confers_no_fulfillment_or_readiness_duty).
narrative_ontology:cs_axiom_status(suspended_law_study_confers_no_fulfillment_or_readiness_duty, holdable).
narrative_ontology:cs_axiom_grounding('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', suspended_law_study_confers_no_fulfillment_or_readiness_duty, conventional).
narrative_ontology:cs_reference_frame('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', altar_contingent_validity).
narrative_ontology:cs_drift_state('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', contemporary_post_print_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9bc81ef-14ac-4b43-b6d9-078fcc7c0e2c', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, rabbinic_academy_establishment).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, kodashim_scholarship_specialists).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, diaspora_lay_communities).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, yeshiva_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, aspiring_halakhic_decisors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, aspiring_halakhic_decisors).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, diaspora_lay_communities).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, mesorah_transmission_authority).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, altar_contingency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the yeshiva curriculum, ordains decisors, and governs which tractates carry prestige. Its claim to steward the whole Torah — including its hardest and least applicable sections — is bound up with what the institution is; dropping the sacrificial order would amount to conceding that its guardianship is partial. Its authority flows from presenting the full corpus as a living inheritance rather than an archive.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, rabbinic_academy_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Elite scholars whose reputations rest on mastery of the sacrificial tractates, the most technically demanding and least practically exercised part of the curriculum. They collect honor, advanced students, and institutional positions keyed to the specialization. Decades of investment mean that redirection would amount to discarding their life's work.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, kodashim_scholarship_specialists, beneficiary,
    organized, biographical, identity_locked, global).

% Spend the prime cognitive years of adolescence and early adulthood working through sacrificial-law tractates that offer no occasion for practice. Progression, stipends, matchmaking prospects, and communal standing all run through the standard sequence; opting out of the kodashim block marks a student as unserious. Individual exit means leaving the community's educational track entirely.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_students, payer,
    powerless, biographical, constrained, global).

% Train for certification in applied law while the curriculum reserves substantial years for sacrificial material they will rarely adjudicate. Mastery of the full corpus signals thoroughness and opens senior positions, so the same material that dilutes their practical training also credentials them for advancement.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, aspiring_halakhic_decisors, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__performance_only, aspiring_halakhic_decisors, beneficiary).

% Fund the academies and receive continuity goods: the assurance that the whole inherited law, altar or no altar, is being kept alive. Their donations sustain the study halls and their children supply the students. Disaffiliation is possible but severs families from the communal fabric that organizes schooling, marriage, and standing.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, diaspora_lay_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__performance_only, diaspora_lay_communities, payer).

% Small activist groups preparing vessels, sites, and priestly lineages for a rebuilt Temple. They reject the premise that the sacrificial commandment is dormant and would redirect resources toward concrete restoration work; they sit outside the curriculum-setting bodies and are treated as marginal enthusiasts by the academies.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, temple_restoration_movements, excluded,
    organized, biographical, trapped, regional).

% University-based historians and philologists who document how the sacrificial corpus was preserved, taught, and ritualized after 70 CE. They describe the arrangement's history, economics, and textual stability without participating in its authority structure or depending on its credentials.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, academic_jewish_studies_scholars, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__performance_only, rabbinic_academy_establishment).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transmission of a fixed legal corpus: a shared canonical sequence of texts, a common standard of mastery, and a single interpretive authority structure that binds dispersed communities into one continuous conversation. Whatever else it does, the arrangement solves the problem of keeping one canon, one chain of accreditation, and one communal self-description intact across exile and dispersion.
% TRANSFER_FUNCTION: Moves years of student labor and communal funding into specialized study of legislation with no current occasion for performance; moves honor, institutional position, and agenda-setting authority to the specialists and administrators who master and control that material; moves a continuity assurance to the lay funders who sustain it.
% ABSENT_VOICES: Temple-restoration activists, who would contest the dormancy premise and redirect effort toward physical preparation, sit outside the curriculum-setting bodies. Applied-halakha educators and communal budget planners who would rebalance curricular time toward decisor-relevant material have no formal seat. Women historically excluded from yeshiva study had no voice in what the curriculum demanded of anyone. Prospective learners deterred by the corpus's demands are unrepresented.
% DISAPPEARANCE_RATIONALE: If the arrangement ended overnight, yeshiva curricula would rebalance toward applicable law within a generation, specialist prestige tracks would dissolve, liturgical rehearsal of the service would lose its pedagogical anchor, and the community's account of itself as keeper of the entire law would need rewriting. Funding flows, career ladders, and marriage-market signaling would all reorganize around the change.
% FOUNDING_PROBLEM: After the Second Temple's destruction in 70 CE, the commandments tied to the altar became impossible to perform, and the rabbinic leadership faced the prospect that the detailed sacrificial legislation — a large fraction of the written and oral law — would be forgotten within a few generations. The study arrangement was built to preserve that legislation intact through exile.
% FOUNDING_PROBLEM_CORROBORATION: Historians of rabbinic literature outside the benefiting parties attest the preservation goal was achieved: the sacrificial corpus has been textually stable in print since the early modern period, is universally accessible in critical editions, and no longer depends on any living chain of memorization. Temple-restoration organizations maintain independent study programs from the same printed sources, showing the material survives without the academies' custodial role. No source outside the benefiting parties attests that forgetting remains a live risk.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74) because the arrangement consumes the prime cognitive years of thousands of students per generation on material with no performable application, and the opportunity cost — against applied law, textual work, or communal needs — is large and borne by those with the least say. Suppression (0.62) is real but non-physical: exit runs through family expectation, stipend dependence, matchmaking markets, and communal standing, deepened by identity fusion with the duty-frame. Theater (0.60) is substantial and rising: the liturgy rehearses an order of service that cannot occur, advanced study elaborates hypothetical temple architecture and priestly procedure with no referent, and completion cycles celebrate mastery of the inert block. Accessibility_collapse (0.40) is moderate-low: alternatives within the system — applied halakha, Bible, ethics — remain open, so the arrangement weights the curriculum rather than monopolizing it. Resistance (0.42) is muted inside the commitment system (periodic reform proposals, attrition), though the historical record includes wholesale external rejections predating the measured interval. All three temporal series share one grid; suppression is nearly flat because the enforcement machinery changed little — the drift is in extraction and theater, driven by institutional growth (kollel expansion, lengthening study careers) rather than by hardening coercion. On Boltzmann typing: the declared identity_coordination function (boundary maintenance, membership marking through shared canonical mastery) is genuine but partially serves as cover; the type's complexity offset should not excuse coupling that concentrates burden on powerless students at global scope while authority concentrates at the administrative seat. Student powerlessness is individual, not aggregate — the coalition check notes historical precedents (Musar-era curricular insertions, post-war rebuilding choices) where collective pressure did move the curriculum.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the establishment seat the arrangement is the tradition's continuity itself — the guardianship claim that constitutes its authority — so it experiences the structure as load-bearing coordination it built and maintains. From the student seat the same structure is a decade of compulsory labor on unusable law with gated exits. Specialists occupy a third position: subsidized by the prestige economy yet unable to leave it without forfeiting their life's work. Trainee decisors straddle target and beneficiary positions, paying in diluted practical training while collecting corpus-credential value. The engine derives these divergences from power, exit, and role declarations; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the academy establishment and the specialist scholars near the beneficiary pole — the arrangement subsidizes their authority and standing — but both carry identity_locked exit, which keeps them structurally engaged rather than free-floating. Lay communities hold a dual position: continuity beneficiaries who also fund the system, deriving a middling directionality. Victim declarations place students and trainee decisors near the target pole, with constrained (not trapped) exit — leaving is possible but priced in community membership. Because suppression is a raw unscaled property while extractiveness scales with directionality and scope, the global scope of the yeshiva network modestly amplifies effective extraction on the payer seats while the establishment's derived directionality damps it toward subsidy. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms already separate the seats the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preservation against forgetting — is dead: the corpus has been print-stable and universally accessible for centuries, corroborated by historians outside the benefiting parties. Yet the arrangement persists and the world would rearrange without it, which is exactly the dead-problem-plus-world_rearranges mismatch the R5 consumer flags. Reading the mismatch through this reading's structure prevents two mislabels: calling the arrangement pure coordination ignores that its mandate is accomplished and its gains accrue to a capturable seat; calling it mere inertia ignores that a concentrated administrator actively maintains, expands, and benefits from it. Receipt: the diverted labor and funding convert into agenda control and guardianship authority at the establishment seat, which is why gain_flow names it; fixing is prohibitive because rebalancing the curriculum would concede that the guardianship claim is partial — costing the only actor who could fix it its constitutive identity, for a benefit diffused across everyone else. The honest resolution under this reading is capture of a completed mandate, computed by the engine from the structural data rather than asserted by the origin story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the kodashim_commandment_status kernel correctly characterizes the standing arrangement — husk (this file), dormant readiness (messianic_deferral), or transposed fulfillment (study_as_performance)?',
    'Not resolvable by data alone: the readings assign rival normative statuses to the same corpus. Resolution comes from which framework the community''s authoritative interpreters adopt; behavioral evidence (whether authorities treat study as discharging an obligation, or as mere preparation) discriminates study_as_performance from the other two.',
    'If messianic_deferral is adopted, extractiveness drops sharply — study becomes forward-looking readiness investment with a live function. If study_as_performance is adopted, the arrangement approaches pure coordination and the victim set largely dissolves. This file''s high-extraction profile holds only under the performance_only reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Reading-choice ambiguity within the kernel; the classification is conditional on this reading being the operative one.').

omega_variable(
    restoration_contingency,
    'Will the Temple and altar ever be restored, converting the husk back into live law?',
    'Only the event itself resolves it; no intermediate data short of physical reconstruction settles the question.',
    'Restoration would collapse this reading''s extraction profile overnight — the diverted labor becomes direct preparation and the victim set converts to investors. Permanent non-restoration leaves the profile as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_contingency, empirical, 'Eschatological contingency governing whether the suspension is permanent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional gatekeeping, economic dependence) or internalized (duty-fusion, identity), and in what proportion?',
    'Post-exit trajectory studies of alumni who left the educational track: if the obligation-sense and curriculum-normativity persist after all structural barriers are removed, a large share is internalized.',
    'If predominantly internalized, effective suppression exceeds the structural measure — leavers carry the arrangement''s demands with them — and remedies targeting institutional rules alone underperform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression split for the student seat.').

omega_variable(
    redirected_effort_value,
    'What would the diverted scholarly labor actually produce if redirected — is the counterfactual alternative use real?',
    'Compare output profiles of cohorts and institutions that weighted the sacrificial block lightly (applied-law concentrations, academic programs) against matched heavy-weighting cohorts.',
    'If the counterfactual uses are low-value, the measured extraction overstates harm and the arrangement looks closer to costly-but-benign maintenance; if high-value, the victim claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redirected_effort_value, empirical, 'Opportunity-cost reality check on the victim set.').

omega_variable(
    capture_vs_inertia,
    'Does the establishment seat capture the arrangement''s gains (active capture) or merely administer an inherited structure it lacks the will to dismantle (inertia)?',
    'Decision-point analysis: examine occasions when the establishment chose to expand versus rebalance the sacrificial footprint (kollel growth decisions, curriculum reforms) and whether expansion tracked institutional interest.',
    'Capture supports the snare-flavored reading with a named receipt seat; pure inertia would push toward a piton-like profile with diffuse gains and would weaken the receipt-surface claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_vs_inertia, conceptual, 'Whether gains are captured by the administrator or diffusely inherited.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t1948, kodashim_commandment_status__performance_only, theater_ratio, 1948, 0.38).
narrative_ontology:measurement_basis(koda_tr_t1948, observed).
narrative_ontology:measurement(koda_tr_t1967, kodashim_commandment_status__performance_only, theater_ratio, 1967, 0.42).
narrative_ontology:measurement_basis(koda_tr_t1967, observed).
narrative_ontology:measurement(koda_tr_t1980, kodashim_commandment_status__performance_only, theater_ratio, 1980, 0.47).
narrative_ontology:measurement_basis(koda_tr_t1980, observed).
narrative_ontology:measurement(koda_tr_t1993, kodashim_commandment_status__performance_only, theater_ratio, 1993, 0.51).
narrative_ontology:measurement_basis(koda_tr_t1993, observed).
narrative_ontology:measurement(koda_tr_t2005, kodashim_commandment_status__performance_only, theater_ratio, 2005, 0.55).
narrative_ontology:measurement_basis(koda_tr_t2005, observed).
narrative_ontology:measurement(koda_tr_t2015, kodashim_commandment_status__performance_only, theater_ratio, 2015, 0.58).
narrative_ontology:measurement_basis(koda_tr_t2015, observed).
narrative_ontology:measurement(koda_tr_t2025, kodashim_commandment_status__performance_only, theater_ratio, 2025, 0.6).
narrative_ontology:measurement_basis(koda_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t1948, kodashim_commandment_status__performance_only, base_extractiveness, 1948, 0.52).
narrative_ontology:measurement_basis(koda_be_t1948, observed).
narrative_ontology:measurement(koda_be_t1967, kodashim_commandment_status__performance_only, base_extractiveness, 1967, 0.57).
narrative_ontology:measurement_basis(koda_be_t1967, observed).
narrative_ontology:measurement(koda_be_t1980, kodashim_commandment_status__performance_only, base_extractiveness, 1980, 0.61).
narrative_ontology:measurement_basis(koda_be_t1980, observed).
narrative_ontology:measurement(koda_be_t1993, kodashim_commandment_status__performance_only, base_extractiveness, 1993, 0.65).
narrative_ontology:measurement_basis(koda_be_t1993, observed).
narrative_ontology:measurement(koda_be_t2005, kodashim_commandment_status__performance_only, base_extractiveness, 2005, 0.69).
narrative_ontology:measurement_basis(koda_be_t2005, observed).
narrative_ontology:measurement(koda_be_t2015, kodashim_commandment_status__performance_only, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement_basis(koda_be_t2015, observed).
narrative_ontology:measurement(koda_be_t2025, kodashim_commandment_status__performance_only, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(koda_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t1948, kodashim_commandment_status__performance_only, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement_basis(koda_su_t1948, observed).
narrative_ontology:measurement(koda_su_t1967, kodashim_commandment_status__performance_only, suppression_requirement, 1967, 0.56).
narrative_ontology:measurement_basis(koda_su_t1967, observed).
narrative_ontology:measurement(koda_su_t1980, kodashim_commandment_status__performance_only, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement_basis(koda_su_t1980, observed).
narrative_ontology:measurement(koda_su_t1993, kodashim_commandment_status__performance_only, suppression_requirement, 1993, 0.59).
narrative_ontology:measurement_basis(koda_su_t1993, observed).
narrative_ontology:measurement(koda_su_t2005, kodashim_commandment_status__performance_only, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement_basis(koda_su_t2005, observed).
narrative_ontology:measurement(koda_su_t2015, kodashim_commandment_status__performance_only, suppression_requirement, 2015, 0.61).
narrative_ontology:measurement_basis(koda_su_t2015, observed).
narrative_ontology:measurement(koda_su_t2025, kodashim_commandment_status__performance_only, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(koda_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'status of the sacrificial laws': one kernel, three structurally distinct constraints. This file (performance_only) authors high epsilon against the standing study arrangement; messianic_deferral authors lower epsilon (a live readiness function); study_as_performance authors near-zero extraction (study IS the performance). All three cite the same fixed corpus as warrant, so the family links run kernel-mate to kernel-mate; every member links the others per the epsilon-invariance principle, and no single story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
