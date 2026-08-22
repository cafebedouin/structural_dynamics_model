% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Fixed-Ratification-Meaning Constraint on Constitutional Adjudication (Originalist Reading)
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   Since roughly 1970, an interpretive regime has consolidated in American
 *   federal courts under which constitutional provisions carry the meaning
 *   their words conveyed to the ratification-era public, and judges must
 *   recover that meaning from historical evidence rather than update it;
 *   post-ratification practice counts only insofar as it evidences original
 *   understanding. The regime presents itself as democratic fidelity — judges
 *   apply law the people enacted, not the judges' own values. This story
 *   models that regime AS the constraint, from the analytical seat: it has a
 *   genuine coordination core (binding judicial discretion to an external
 *   standard) and an asymmetric extraction structure (claims lacking
 *   founding-era pedigree systematically lose; a movement built around the
 *   method collects appointments, funding, and doctrinal wins). Per the
 *   committer frame, this file instantiates ONE reading of the
 *   us_constitution_text kernel — the originalist reading; the
 *   living-constitutionalist and positivist readings are separate constraints
 *   with their own epsilon, beneficiaries, and victims, linked through
 *   network.affects_constraints. The claim/metric split is deliberate:
 *   claimed_type records tangled_rope as the structure I believe true; the
 *   metrics record the operation I believe descriptively accurate. KEY AGENTS
 *   (by structural relationship): - supreme_court_originalist_majority:
 *   Agenda-setter (institutional/arbitrage) — applies and calibrates the
 *   method - conservative_legal_movement: Primary beneficiary
 *   (organized/identity_locked) — collects appointments, funding, doctrinal
 *   wins; runs the personnel pipeline -
 *   elected_branches_shielded_by_deference: Secondary beneficiary
 *   (powerful/mobile) — legislation survives review more often -
 *   unenumerated_rights_claimants: Primary target (powerless/trapped) —
 *   claims without founding-era pedigree fail -
 *   adaptive_interpretation_proponents: Target (moderate/constrained) —
 *   professionally marginalized - lower_court_judges: Payer
 *   (powerful/constrained) — bears compliance costs and reversal risk -
 *   future_generations_facing_novel_circumstances: Excluded voice
 *   (powerless/trapped) — absent from the evidentiary record that binds them
 *   - legal_historians: Analytical observer (moderate/analytical) — supplies
 *   and audits the evidence
 *
 * KEY AGENTS:
 *   - supreme_court_originalist_majority: agenda-setter seat, institutional power, arbitrage exit — controls the method's application
 *   - conservative_legal_movement: primary beneficiary, organized power, identity_locked exit — collects the regime's gains and staffs its referees
 *   - elected_branches_shielded_by_deference: secondary beneficiary, powerful, mobile — enjoys restrained judicial review
 *   - unenumerated_rights_claimants: primary target, powerless, trapped — loses claims the historical record cannot support
 *   - adaptive_interpretation_proponents: target, moderate, constrained — pushed outside the appointment and publication mainstream
 *   - lower_court_judges: payer, powerful, constrained — must comply without having chosen the method
 *   - future_generations_facing_novel_circumstances: excluded, powerless, trapped — no seat in the inquiry that fixes their rights
 *   - legal_historians: observer, moderate, analytical — audit the evidentiary base without deciding anything
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.78).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Fixed-Ratification-Meaning Constraint on Constitutional Adjudication (Originalist Reading)").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, 'da170005-0473-48e1-afcc-7e4d04c8a202').
narrative_ontology:cs_kernel_codification('da170005-0473-48e1-afcc-7e4d04c8a202', fixed_text).
narrative_ontology:cs_authority_grounding('da170005-0473-48e1-afcc-7e4d04c8a202', lineage).
narrative_ontology:cs_interpretation_layer_present('da170005-0473-48e1-afcc-7e4d04c8a202').
narrative_ontology:cs_reading_relation('da170005-0473-48e1-afcc-7e4d04c8a202', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('da170005-0473-48e1-afcc-7e4d04c8a202', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('da170005-0473-48e1-afcc-7e4d04c8a202', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('da170005-0473-48e1-afcc-7e4d04c8a202', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('da170005-0473-48e1-afcc-7e4d04c8a202', secondary, judges_may_not_impose_unenacted_values).
narrative_ontology:cs_axiom_status(judges_may_not_impose_unenacted_values, holdable).
narrative_ontology:cs_axiom_grounding('da170005-0473-48e1-afcc-7e4d04c8a202', judges_may_not_impose_unenacted_values, deontological).
narrative_ontology:cs_reference_frame('da170005-0473-48e1-afcc-7e4d04c8a202', fixed_ratification_public_meaning).
narrative_ontology:cs_drift_state('da170005-0473-48e1-afcc-7e4d04c8a202', contemporary_stare_decisis_accommodation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da170005-0473-48e1-afcc-7e4d04c8a202', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, elected_branches_shielded_by_deference).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, adaptive_interpretation_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, lower_court_judges).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, popular_sovereignty_fixity_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, counter_majoritarian_difficulty_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which historical sources count, how much weight precedent carries against original meaning, and when history-and-tradition tests govern. Controls the method's application case by case and can narrow or widen it. Members reached the bench through a selection process that screened for commitment to the method; abandoning it would repudiate the credentials that produced their seats.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, supreme_court_originalist_majority, agenda_setter,
    institutional, generational, arbitrage, national).

% A network of lawyers, academics, judges-in-waiting, and donor-funded organizations built since the 1980s around textualist and originalist method. It staffs the appointment pipeline, trains clerks, funds centers and journals, and supplies the historical briefs courts cite. Its members' careers, funding, and status are bound to the method's continued dominance; abandoning it would dissolve the professional identity the network confers.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, conservative_legal_movement, agenda_setter).

% Federal and state legislators enact policy knowing that courts applying the method strike down fewer modern regulatory and rights-expanding measures than courts weighing contemporary consequences would. They bear little direct cost and retain formal amendment as a theoretical remedy, though that path is almost never taken.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, elected_branches_shielded_by_deference, beneficiary,
    powerful, biographical, mobile, national).

% Litigants whose claims rest on liberty, equality, or dignity interests without clear founding-era analogues — new family forms, digital privacy, medical autonomy. Their cases turn on whether judges find a historical tradition protecting them; where none existed in 1791 or 1868, they lose regardless of the strength of their contemporary interest. Litigation is their only avenue, and there is no exit from the governing method except losing.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Scholars, advocates, and some state judges who argue that meaning develops with application. They publish, litigate, and dissent against the method and are increasingly outside the appointment pipeline and the elite-journal mainstream; their professional advancement now runs through institutions whose gatekeeping the method controls. Switching camps is possible but means surrendering a body of work.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, adaptive_interpretation_proponents, payer,
    moderate, generational, constrained, national).

% District and circuit judges must apply the method as handed down, commissioning historical analysis in ordinary cases without the research staff or time the task presumes, and risk reversal when their historical synthesis differs from the Supreme Court's. They did not choose the method and cannot decline it.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, lower_court_judges, payer,
    powerful, biographical, constrained, national).

% People who will confront technologies, social arrangements, and threats the ratifying public never imagined. The evidentiary record the method consults contains nothing about them, and no mechanism aggregates their interests into the historical inquiry that decides their rights.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, future_generations_facing_novel_circumstances, excluded,
    powerless, civilizational, trapped, universal).

% Professional historians whose archival work supplies the evidence courts invoke and who increasingly publish critiques of how that evidence is selected and characterized in opinions. They hold no vote in doctrine and bear neither the wins nor the losses; their stake is the accuracy of the record.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, legal_historians, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single external interpretive target — the text's ratification-era public meaning — so that litigants, legislatures, and citizens can predict what the Constitution permits without consulting the moral views of whoever sits on the bench; binds all judges, including unfavored ones, to the same standard.
% TRANSFER_FUNCTION: Moves interpretive authority from sitting judges' contemporary moral judgment to historical evidence and its professional custodians; in outcome terms, moves wins in constitutional litigation away from claims requiring evolved or novel normative premises and toward claims anchored in founding-era practice; in career terms, moves judicial seats, clerkships, and scholarly prestige toward practitioners fluent in the method.
% ABSENT_VOICES: Future generations confronting circumstances absent from the founding-era record have no seat in the inquiry that fixes their rights; the enslaved, women, and the unpropertied — excluded from the ratification-era 'public' whose understanding binds everyone — remain structurally absent from the evidentiary base the method privileges. Both groups would object that the constraint generalizes obligations from a conversation they were never admitted to.
% DISAPPEARANCE_RATIONALE: If the fixed-meaning requirement vanished overnight, doctrine would shift within a few terms: precedents resting on history-and-tradition tests would be open to reargument under consequence-sensitive reasoning, the movement's appointment advantage would degrade into a mere stylistic preference, and rights litigation would reorganize around contemporary-harm showings instead of historical-pedigree hunts.
% FOUNDING_PROBLEM: The perceived excesses of mid-twentieth-century adjudication — courts invalidating democratically enacted policy on grounds not traceable to enacted text — posed the counter-majoritarian question: by what warrant do unelected judges override legislation? The modern movement was built to answer: only by enforcing law the people actually enacted, as they understood it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: political-science and legal-history accounts of the movement's post-1970s institutional construction (e.g., Steven Teles's history of the conservative legal movement, confirmation-hearing records) attest that the founding problem was the Warren Court's perceived legitimacy deficit, and mainstream journals still debate whether judicial supremacy over meaning remains a live threat. The movement itself attests the problem is live; prominent critics inside the academy attest it was solved by other mechanisms and the method now persists as a factional instrument — the dispute is externally documented, not merely self-asserted.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the transfer is substantial — whole classes of claims fail wholesale wherever the founding-era record is silent, and the method's winner-take-most structure converts historical silence into permanent defeat — tempered by the fact that the standard binds all judges, including ones the dominant coalition dislikes. Suppression 0.78 is a raw structural property, unscaled by power or scope: enforcement runs through appointment screening, confirmation politics, the reversal hierarchy, and journal/clerkship gatekeeping. Theater 0.40: genuine historical scholarship occurs, but a growing share of courtroom 'history' is litigation-assembled advocacy dressed as archive. Accessibility_collapse 0.45: alternatives persist (state constitutional law, academic living-tradition scholarship, occasional concurrences), so the field has not closed. Resistance 0.60: sustained academic opposition, recurring dissents, and state court divergence. Measurements run on ONE shared grid — time points index years since 1970 across the interval 1970-2025 — with every tracked metric authored at every point. The suppression_requirement series is authored deliberately: enforcement capacity is the dynamic this story traces, as the method moved from fringe persuasion to controlling doctrine to hardened confirmation gatekeeping; extraction and theater ratchet upward along the same arc as the appointment pipeline matured and the stakes of doctrinal control rose.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently by construction. From inside the Court and the movement, the constraint is discipline: it prevents usurpation, and every judge — friendly or hostile — faces the same historical standard. From the claimant seat, the identical structure is a locked door: the same fixity that restrains judges also forecloses claims the eighteenth and nineteenth centuries could not have imagined. Same-level lateral divergence appears between lower-court judges and tenured law professors, who hold comparable professional standing yet sit opposite: judges must comply under reversal exposure, professors may dissent behind tenure — differentiated by reversal exposure, not by global power. Movement members experience the regime as vocation; claimants experience it as verdict; historians experience it as a citation practice they can audit but not affect.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: the conservative legal movement collects the regime's gains and is additionally identity-locked (the network's careers, funding, and self-conception are constituted by the method), pushing it firmly toward the beneficiary pole; the elected branches benefit incidentally through restrained review while retaining formal amendment as a remote exit. Declared victims derive high directionality: unenumerated rights claimants are powerless with no exit but litigation itself, and adaptive-interpretation proponents face constrained exit through gatekept institutions. The Court sits near-symmetric-to-beneficiary: it pays nothing, controls the mechanism, and holds arbitrage-grade exit (it defines the method it applies). Legal historians are analytical. No directionality overrides are needed — the beneficiary/victim declarations plus exit atoms already place every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the counter-majoritarian worry that animated the movement's construction — is contested, not dead, so no dead-problem zombie flag fires; the mismatch consumer reads status=contested x verdict=world_rearranges and finds no forced capture signal. The tangled_rope classification guards against both mislabels: calling this a snare ignores the real discretion-binding coordination function that even opponents concede (the standard constrains ALL judges, including ones the dominant coalition dislikes, and supplies the predictability litigants plan around); calling it a rope ignores the systematic loser class and the movement's capture of the referee-selection pipeline. Both facts are structural, and the hybrid category preserves both. If the founding problem were later shown dead — judicial supremacy over meaning dissolved by other legitimacy mechanisms — while the world still rearranges around the method, the mismatch flag would fire and piton-or-zombie analysis would follow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the operative constraint on American constitutional adjudication this reading (meaning fixed at ratification), the living reading (meaning evolves with application), or the positivist reading (validity from enactment procedure alone)?',
    'Doctrinal analysis of which method controlling majorities actually apply across issue areas, plus professional-consensus indicators: casebook framing, faculty hiring patterns, state court adoption rates.',
    'Each sibling reading instantiates a different constraint with a different victim set: under the living reading the targets are entrenchment-seeking actors rather than rights claimants; under the positivist reading historical pedigree becomes irrelevant and this story''s extraction profile collapses toward zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the constitutional-text kernel actually binds adjudication.').

omega_variable(
    original_meaning_determinacy,
    'Is founding-era public meaning determinate enough to constrain judges, or does pervasive historical ambiguity leave room for motivated selection behind a facade of evidence?',
    'Corpus-linguistic and founding-era database projects measuring convergence of period usage; inter-rater reliability studies on independent originalist analyses of the same clauses.',
    'If meaning is largely indeterminate, the method functions as discretion wearing historical costume — extraction and theater rise and the classification trends toward pure extraction; if determinate, the coordination function is stronger than the metrics assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_determinacy, empirical, 'Determinacy of the historical evidence the method treats as binding.').

omega_variable(
    judicial_history_quality,
    'Is the history deployed in controlling opinions sound by professional-historical standards, or adversarial cherry-picking assembled for litigation?',
    'Systematic peer review of the historical claims in landmark opinions by credentialed historians uninvolved in the underlying litigation.',
    'Widespread unsound history raises theater_ratio sharply (rigor performed rather than practiced) and increases effective extraction on litigants subjected to unreliable tests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_history_quality, empirical, 'Quality of the historical record courts actually rely on.').

omega_variable(
    method_vs_factional_capture,
    'Does the current regime represent the method applied in good faith, or factional capture operating through the method''s vocabulary?',
    'Blinded panels scoring the relative historical strength of each side in decided cases, then testing whether outcomes correlate with appointing-coalition preference after controlling for scored historical merit.',
    'If capture dominates, gains concentrate further in the movement seat and the arrangement trends toward pure extraction maintained by appointment control; if the method binds, the coordination component is real and durable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(method_vs_factional_capture, conceptual, 'Good-faith method versus cover-story capture.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of adaptive interpretation structural (appointment gates, reversal risk, journal and clerkship gatekeeping) or internalized (judges and scholars treating adaptive argument as presumptively illegitimate)?',
    'Post-relaxation trajectory: if adaptive argument revives immediately wherever enforcement loosens (state courts, concurrences, doctrinal footnotes), suppression was structural; if advocates continue self-censoring after gates open, it is internalized.',
    'Internalized suppression outlasts enforcement machinery — removing the appointment gate alone would not restore adaptive interpretation, extending the persistence of extraction beyond the visible enforcement apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orig_reading_meas_tr_t0, us_constitution_text__originalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(orig_reading_meas_tr_t10, us_constitution_text__originalist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(orig_reading_meas_tr_t20, us_constitution_text__originalist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(orig_reading_meas_tr_t30, us_constitution_text__originalist_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(orig_reading_meas_tr_t40, us_constitution_text__originalist_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(orig_reading_meas_tr_t50, us_constitution_text__originalist_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(orig_reading_meas_tr_t55, us_constitution_text__originalist_reading, theater_ratio, 55, 0.4).

% Extraction over time
narrative_ontology:measurement(orig_reading_meas_be_t0, us_constitution_text__originalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(orig_reading_meas_be_t10, us_constitution_text__originalist_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(orig_reading_meas_be_t20, us_constitution_text__originalist_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(orig_reading_meas_be_t30, us_constitution_text__originalist_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(orig_reading_meas_be_t40, us_constitution_text__originalist_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(orig_reading_meas_be_t50, us_constitution_text__originalist_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(orig_reading_meas_be_t55, us_constitution_text__originalist_reading, base_extractiveness, 55, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(orig_reading_meas_su_t0, us_constitution_text__originalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(orig_reading_meas_su_t10, us_constitution_text__originalist_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(orig_reading_meas_su_t20, us_constitution_text__originalist_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(orig_reading_meas_su_t30, us_constitution_text__originalist_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(orig_reading_meas_su_t40, us_constitution_text__originalist_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(orig_reading_meas_su_t50, us_constitution_text__originalist_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(orig_reading_meas_su_t55, us_constitution_text__originalist_reading, suppression_requirement, 55, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional interpretation' decomposes into three reading-stories of the us_constitution_text kernel because the label conflates structurally distinct claims with different epsilon, victim sets, and failure modes. This (originalist) story links to both siblings; resource flow among them runs through the shared appointment pipeline — whichever reading controls nominations reshapes the others' operating environment without foreclosing them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
