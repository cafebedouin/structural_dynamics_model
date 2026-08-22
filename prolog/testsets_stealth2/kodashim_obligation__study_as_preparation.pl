% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Study-as-Preparation Obligation (Binding but Unperformable)
 *   domain: religious/legal/textual-preservation
 *
 * SUMMARY:
 *   A Torah-level body of law — the Kodashim, the sacrificial orders of the
 *   Mishnah and Talmud — remains binding on the covenant community roughly
 *   two millennia after the destruction of the Second Temple removed the only
 *   venue in which it can be performed. On the reading instantiated here, the
 *   obligation is real and undischarged: no substitute (not even study)
 *   discharges it, the cosmic repair the service would effect is deferred,
 *   and the community sustains a standing regime of technical study —
 *   species, procedures, priestly duties, disqualification rules — whose
 *   designated purpose is preservation for a messianic restoration at which
 *   performance resumes. The epsilon referent is the standing arrangement
 *   (the binding obligation plus the study regime that keeps it alive),
 *   assessed by this reading's own lights — never the restored arrangement
 *   the reading anticipates. This story is one reading of the
 *   kodashim_obligation kernel; the sibling readings and the location of the
 *   disagreement are recorded in kernel_context and the omegas. KEY AGENTS
 *   (by structural relationship): - present_generation_of_israel: Primary
 *   target (organized/identity_locked) — bears the undischarged obligation
 *   and the study labor - messianic_era_community: Primary beneficiary
 *   (powerless/trapped) — inherits the preserved knowledge and the restored
 *   service - rabbinic_academy_system: Agenda setter
 *   (institutional/constrained) — administers the curricula and study cycles
 *   - temple_preparation_movement: Dual-positioned
 *   (organized/identity_locked) — collects purpose and standing from the
 *   anticipation while administering concrete preparation -
 *   secular_judaic_studies_scholars: Excluded voice (moderate/mobile) — reads
 *   Kodashim as archive, holds no seat in halakhic adjudication -
 *   religion_historians: Analytical observer (analytical/analytical) —
 *   documents the preservation project without a stake in the contest
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.28).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.32).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.28).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Study-as-Preparation Obligation (Binding but Unperformable)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/legal/textual-preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '84a2a11c-5a72-406e-a766-48463b35d827').
narrative_ontology:cs_kernel_codification('84a2a11c-5a72-406e-a766-48463b35d827', fixed_text).
narrative_ontology:cs_authority_grounding('84a2a11c-5a72-406e-a766-48463b35d827', lineage).
narrative_ontology:cs_interpretation_layer_present('84a2a11c-5a72-406e-a766-48463b35d827').
narrative_ontology:cs_reading_relation('84a2a11c-5a72-406e-a766-48463b35d827', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('84a2a11c-5a72-406e-a766-48463b35d827', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_axiom('84a2a11c-5a72-406e-a766-48463b35d827', foundational, sacrificial_law_remains_binding).
narrative_ontology:cs_axiom_status(sacrificial_law_remains_binding, holdable).
narrative_ontology:cs_axiom_grounding('84a2a11c-5a72-406e-a766-48463b35d827', sacrificial_law_remains_binding, theological).
narrative_ontology:cs_axiom('84a2a11c-5a72-406e-a766-48463b35d827', foundational, study_preserves_for_restoration).
narrative_ontology:cs_axiom_status(study_preserves_for_restoration, holdable).
narrative_ontology:cs_axiom_grounding('84a2a11c-5a72-406e-a766-48463b35d827', study_preserves_for_restoration, instrumental).
narrative_ontology:cs_axiom('84a2a11c-5a72-406e-a766-48463b35d827', secondary, temple_restoration_structurally_required).
narrative_ontology:cs_axiom_status(temple_restoration_structurally_required, holdable).
narrative_ontology:cs_axiom_grounding('84a2a11c-5a72-406e-a766-48463b35d827', temple_restoration_structurally_required, theological).
narrative_ontology:cs_reference_frame('84a2a11c-5a72-406e-a766-48463b35d827', sacrificial_performance_normative_default).
narrative_ontology:cs_drift_state('84a2a11c-5a72-406e-a766-48463b35d827', contemporary_post_destruction_exile, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('84a2a11c-5a72-406e-a766-48463b35d827', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_era_community).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, present_generation_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, temple_preparation_movement).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, halakhic_binding_continuity_doctrine).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, messianic_restoration_expectation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Born into a covenant that binds it to a sacrificial law it cannot perform: the Temple is absent, so the obligation stands undischarged for a lifetime, and the cosmic repair the service would effect is deferred to a restoration most of its members will not witness. It sustains the study regime that keeps the technical law alive and carries the standing weight of a debt it cannot pay. Its exit — ceasing to hold the obligation as binding — is equivalent to leaving the covenantal community that constitutes its identity.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, present_generation_of_israel, payer,
    organized, biographical, identity_locked, global).

% The future generation for whom the preserved knowledge is destined. It contributes nothing to the present arrangement, cannot advocate for itself, and has no exit from the arrangement it will inherit; its anticipated needs discipline present study. It receives the restored Temple service and the preserved technical law only if the restoration its elders anticipate arrives.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_era_community, beneficiary,
    powerless, civilizational, trapped, global).

% Sets curricula, runs the study cycles that carry the sacrificial orders (tractate-by-tractate cycles, yeshiva syllabi), and teaches the preparation rationale: the law is studied because performance will resume. It bears the administrative cost of keeping inapplicable technical law in active circulation and collects incidental sustenance and standing from the study economy, while the reading's designated product — preserved knowledge — routes past it to the future.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_academy_system, agenda_setter,
    institutional, generational, constrained, global).

% A small organized movement that operationalizes the preparation reading: fabricates service vessels, trains priestly families, and maintains readiness inventories for a restored service. It collects purpose, funding, and standing from the restoration-anticipation the obligation sustains, and it administers the concrete preparation programs. Its existence is constituted by the anticipation; abandoning it would dissolve the movement.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, temple_preparation_movement, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, temple_preparation_movement, agenda_setter).

% Study Kodashim as the documentary record of a defunct cult rather than a binding legal system. They would object that the preparation framing mistakes a literary archive for a live obligation, but they hold no seat in halakhic adjudication and their reading lives outside the covenantal frame.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, secular_judaic_studies_scholars, excluded,
    moderate, biographical, mobile, global).

% Document the knowledge-preservation project's actual history — the redaction of the sacrificial orders, the continuous curricular presence of Kodashim, the modern preparation movements — without holding any position in the contest over the obligation's binding force.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, religion_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, messianic_era_community).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps the complete technical specification of a sacrificial system — species, procedures, priestly duties, disqualification rules — alive in active memory and curriculum across a dispersed community during an indefinite period in which the system cannot be operated, so that a restoration would not begin from zero.
% TRANSFER_FUNCTION: Moves the weight of an undischarged binding obligation and the labor of technical study from each present generation forward, accumulating preserved knowledge and ritual readiness that a future, messianic-era community will inherit and use; the present pays, the future collects.
% ABSENT_VOICES: Secular Judaic studies scholars and non-observant Jews, who read Kodashim as the archive of a defunct cult, have no seat in halakhic adjudication. The designated beneficiary — the messianic-era community — is structurally voiceless: it cannot speak for itself, and every claim about its needs is authored by the present parties who bear the study burden.
% DISAPPEARANCE_RATIONALE: If the binding-but-unperformable obligation and its study-as-preparation regime vanished overnight, Kodashim would leave the yeshiva curriculum within a generation or two, the preparation movement would lose its warrant and dissolve, the liturgical and legal memory of a standing covenantal debt would fade, and any future restoration would begin without a preserved technical base — the traditional community's arrangements visibly depend on the arrangement.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, a community held a Torah-level sacrificial obligation with no venue: how does a covenant keep a binding law operative and its technical knowledge intact across an indefinite exile until restoration?
% FOUNDING_PROBLEM_CORROBORATION: The historical core is corroborated from outside the beneficiary set: historians of religion and textual scholars document the post-70 preservation project in the redaction history of the Mishnah and Talmud and the continuous curricular presence of the sacrificial orders. The normative core — that the obligation remains binding — is attested only within the covenantal frame by the very parties it binds; no external source attests bindingness itself, and that absence is itself signal.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope because both required components are present: a genuine coordination function (multi-century preservation of operational technical knowledge in a dispersed community with no practical use that would sustain it naturally — a real collective-action problem) and asymmetric extraction (the present generation bears an undischarged binding obligation plus the study labor; the beneficiary is a future generation that contributes nothing now). The extraction is mild, consistent with a tangled rope at the low end rather than a pure rope, because on this reading's own terms the present generation is not a net beneficiary — study is preparation, not discharge, so the obligation's weight is not offset. Metrics are authored independently of the claim: epsilon 0.28 (low-moderate; no present capturer, the designated product routes to the future); suppression 0.32 (enforcement is communal-educational — curricula, study cycles, social expectation — no coercive machinery exists); theater_ratio 0.14 (the preserved knowledge is real technical content and the preparation function is genuine on the reading's own terms; only the ceremonial study-cycle completions are performative); accessibility_collapse 0.3 (the sibling readings remain live alternatives inside and adjacent to the frame, and many communities lightly de-emphasize Kodashim study); resistance 0.2 (the preparation framing is the mainstream traditional position, contested mainly at the margins). The measurement series run on one shared grid (70, 400, 800, 1200, 1600, 2000, 2026): extraction declined as redaction and curriculum institutionalized preservation and the burden routinized, then partially renewed in the modern era (universal study cycles from 1923; concrete preparation movements after 1967); enforcement intensity declined as the arrangement became self-sustaining through identity and curriculum, then partially renewed; theater stays low throughout because the preservation function remains real. Suppression is authored as a raw structural property — the engine, not this story, scales extractiveness by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seat should compute very different types. From the present generation's position the arrangement is a standing undischarged debt plus a study burden justified by an event most of its members will not witness; from the future's seat — as this reading imagines it — the same structure is the gift that makes restoration possible at all. The academy's seat experiences a curriculum and an institutional identity; the preparation movement's seat experiences purpose and vocation; the excluded archive-readers experience a misclassified library. The identity-lock mechanism on the payer seat is covenantal-ideological: the obligation is constitutive of the community's self-understanding, so exit is not a choice among options but a change of identity — if that identity frame broke, the payer seat's effective position would shift sharply toward the mobile end and the extraction asymmetry would become visible as a choice rather than a condition.
 *
 * DIRECTIONALITY LOGIC:
 *   The present generation is the declared victim with identity_locked exit — it derives near the full-target end of the directionality range, and its global scope and locked exit amplify effective extraction in the engine's computation. The messianic-era community is the declared beneficiary with no present agency — it derives near the full-beneficiary end. The rabbinic academy is the one seat the structural derivation cannot place: it declares no beneficiary or victim position, and the canonical fallback for an undeclared institutional seat could misread it as beneficiary-by-adjacency, so an explicit override sets it at 0.40 — near-symmetric administration with a slight incidental-benefit tilt, because it bears the administrative cost of keeping inapplicable law in circulation and collects standing and sustenance but not the designated product. The preparation movement derives low from its beneficiary declaration; its collections (purpose, funding, standing) are byproducts of the anticipation economy, not the constraint's designated extraction, which is why gain_flow names the future seat rather than either present collector.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Mislabeling as pure rope would erase the intergenerational asymmetry: the present pays, the future collects, and on this reading's own terms study does not discharge the obligation, so the payer is not a net beneficiary. Mislabeling as snare would fabricate a capturer: there is no seat collecting the extraction — the designated beneficiary is future and voiceless, enforcement is educational rather than coercive, and the arrangement declares its own termination condition. The R5 mismatch check comes back clean on this reading's own lights: founding_problem_status is live (the Temple is not rebuilt, the obligation stands, the knowledge still needs keeping) and disappearance_verdict is world_rearranges, so no zombie flag fires — function and persistence are aligned. The live risk is the one the undated_sunset_deferral omega tracks: the sunset is real in the arrangement's self-description but undated in fact, and an arrangement justified by a transition that never arrives is the canonical path from transitional preparation to standing inertia. The contingent_beneficiary_seat omega marks the deeper exposure: if the eschatological premise fails, the coordination story loses its warrant and the mild extraction loses its receiver.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates one reading (study_as_preparation) of the kodashim_obligation kernel; how would the sibling readings restructure the constraint''s victim set, beneficiary, and type?',
    'Compare against the sibling stories: study_as_performance credits study as present discharge, thinning the victim set and dropping extraction toward coordination-cost levels; study_as_archive dissolves the binding claim entirely, leaving historical material rather than a live constraint. The disagreement is located in two structural elements: the function of study (preparation vs. enactment vs. archival) and the binding status of the obligation.',
    'If the performance reading prevails within a community, this constraint''s victim set collapses and its type moves toward rope; if the archive reading prevails, the constraint ceases to be a live constraint at all and the story retires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the kodashim kernel a community holds determines the constraint''s victim set and type.').

omega_variable(
    undated_sunset_deferral,
    'The arrangement declares its own termination condition — messianic restoration — but the condition is undated and possibly indefinitely deferred; is the arrangement transitional in fact, or standing with a rhetorical exit?',
    'Longitudinal comparison with other indefinitely-deferred transitional arrangements: if preparation activity stays functionally alive across generations of deferral, the arrangement operates as designed; if preparation decays into curricular routine while the transitional justification is retained rhetorically, the arrangement is drifting toward inertia.',
    'If the deferral is permanent-in-practice, the transitional justification becomes cover and the constraint drifts from mild intergenerational transfer toward a standing arrangement maintained by curricular habit — the scaffold-shaped sunset question resolved against transitionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(undated_sunset_deferral, conceptual, 'Whether an undated eschatological sunset keeps the arrangement transitional or lets it drift into inertia.').

omega_variable(
    victim_offset_ambiguity,
    'Is the present generation a net victim? This reading does not credit study as discharge of the obligation, but the broader tradition elsewhere grants study-substitution credit; if that credit operates within this reading''s frame, the obligation''s standing weight is offset.',
    'Source analysis: determine whether the preparation reading''s own authorities grant study-substitution credit that discharges the obligation, or whether that credit belongs to the performance reading alone; survey how communities holding this reading describe the obligation''s standing weight in practice.',
    'If the offset applies within this reading, the victim set thins and the constraint moves toward rope; if not, the intergenerational extraction stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_offset_ambiguity, conceptual, 'Whether the undischarged-obligation cost borne by the present generation is offset by study-substitution credit.').

omega_variable(
    contingent_beneficiary_seat,
    'The beneficiary seat is a contingent future generation; if the eschatological premise fails, the arrangement''s gains have no receiver — does the extraction then have payers without a beneficiary?',
    'Not resolvable by evidence in the ordinary sense — the premise is theological. The structural question (what the arrangement is if the beneficiary never arrives) can be clarified by comparison with other arrangements whose designated beneficiaries are contingent futures.',
    'If the beneficiary seat is empty, the arrangement''s extraction loses its receiver and the coordination story loses its warrant; the structure re-reads as burden without return — a materially harsher classification than the one authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contingent_beneficiary_seat, conceptual, 'Whether the constraint''s beneficiary exists depends on an eschatological premise the framework cannot verify.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_obligation__study_as_preparation, theater_ratio, 70, 0.08).
narrative_ontology:measurement(koda_tr_t400, kodashim_obligation__study_as_preparation, theater_ratio, 400, 0.1).
narrative_ontology:measurement(koda_tr_t800, kodashim_obligation__study_as_preparation, theater_ratio, 800, 0.11).
narrative_ontology:measurement(koda_tr_t1200, kodashim_obligation__study_as_preparation, theater_ratio, 1200, 0.12).
narrative_ontology:measurement(koda_tr_t1600, kodashim_obligation__study_as_preparation, theater_ratio, 1600, 0.13).
narrative_ontology:measurement(koda_tr_t2000, kodashim_obligation__study_as_preparation, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(koda_tr_t2026, kodashim_obligation__study_as_preparation, theater_ratio, 2026, 0.14).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_obligation__study_as_preparation, base_extractiveness, 70, 0.38).
narrative_ontology:measurement(koda_be_t400, kodashim_obligation__study_as_preparation, base_extractiveness, 400, 0.32).
narrative_ontology:measurement(koda_be_t800, kodashim_obligation__study_as_preparation, base_extractiveness, 800, 0.27).
narrative_ontology:measurement(koda_be_t1200, kodashim_obligation__study_as_preparation, base_extractiveness, 1200, 0.24).
narrative_ontology:measurement(koda_be_t1600, kodashim_obligation__study_as_preparation, base_extractiveness, 1600, 0.22).
narrative_ontology:measurement(koda_be_t2000, kodashim_obligation__study_as_preparation, base_extractiveness, 2000, 0.26).
narrative_ontology:measurement(koda_be_t2026, kodashim_obligation__study_as_preparation, base_extractiveness, 2026, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_obligation__study_as_preparation, suppression_requirement, 70, 0.45).
narrative_ontology:measurement(koda_su_t400, kodashim_obligation__study_as_preparation, suppression_requirement, 400, 0.4).
narrative_ontology:measurement(koda_su_t800, kodashim_obligation__study_as_preparation, suppression_requirement, 800, 0.35).
narrative_ontology:measurement(koda_su_t1200, kodashim_obligation__study_as_preparation, suppression_requirement, 1200, 0.3).
narrative_ontology:measurement(koda_su_t1600, kodashim_obligation__study_as_preparation, suppression_requirement, 1600, 0.28).
narrative_ontology:measurement(koda_su_t2000, kodashim_obligation__study_as_preparation, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(koda_su_t2026, kodashim_obligation__study_as_preparation, suppression_requirement, 2026, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, information_standard).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, study_as_archive).

% DUAL FORMULATION NOTE:
% The colloquial label 'the obligation to study the sacrificial laws' covers three structurally distinct claims sharing one kernel (the Torah's sacrificial commandments as transmitted through rabbinic lineage). This file instantiates study_as_preparation: binding but unperformable; study is instrumental preparation for restoration; epsilon ~0.28 over a standing-arrangement referent; present generation pays, messianic future collects. Sibling stories: study_as_performance (study enacts the cosmic function now; the Temple's absence is irrelevant; study is discharge, thinning the victim set) and study_as_archive (Kodashim documents a defunct system; no live obligation; epsilon near zero, no victims). The epsilon values differ because the readings disagree about the function of study and the binding status of the obligation; per the epsilon-invariance principle they are authored as separate stories linked through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_obligation__study_as_preparation, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
