% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Adaptive Constitutional Substrate (Living Document Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the living-document reading of the Magna Carta
 *   kernel: the claim that the 1215 charter's authority persists legitimately
 *   not through fidelity to its original 13th-century feudal terms but
 *   through an unbroken chain of judicial reinterpretation and precedential
 *   accumulation that constitutes genuine constitutional development rather
 *   than drift or invention. This is a distinct constraint from the
 *   baronial-privilege reading (which fixes 'free men' to landowning barons
 *   and treats later extension as illegitimate expansion) and the
 *   universal-rights reading (which claims Clause 39 always-already emitted a
 *   transhistorical due-process guarantee for all persons). The
 *   living-document reading makes neither historical claim; instead it makes
 *   a meta-claim about the legitimacy of interpretive authority itself — that
 *   courts may authoritatively update the document's meaning over time and
 *   that doing so is development, not usurpation. Its extraction profile is
 *   moderate: the reading transfers interpretive authority to the judiciary
 *   and standing to whomever precedent has incorporated, at the cost of
 *   textual originalists whose fidelity claims are structurally
 *   disadvantaged.
 *
 * KEY AGENTS:
 *   - common_law_judiciary: institutional authority administering and benefiting from the interpretive tradition
 *   - constitutional_reform_movements: organized beneficiaries of a framework legitimizing doctrinal evolution
 *   - modern_due_process_claimants: moderate-power beneficiaries whose standing depends entirely on precedential extension
 *   - textual_originalist_litigants: payers whose fidelity-to-text claims are structurally disadvantaged
 *   - legal_historians: excluded voices documenting historical discontinuity
 *   - constitutional_theorists: analytical observers of the legitimacy question itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.38).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.32).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Adaptive Constitutional Substrate (Living Document Reading)").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '1423740f-3804-4f93-a28f-a02e8f1cd06d').
narrative_ontology:cs_kernel_codification('1423740f-3804-4f93-a28f-a02e8f1cd06d', fixed_text).
narrative_ontology:cs_authority_grounding('1423740f-3804-4f93-a28f-a02e8f1cd06d', lineage).
narrative_ontology:cs_interpretation_layer_present('1423740f-3804-4f93-a28f-a02e8f1cd06d').
narrative_ontology:cs_reading_relation('1423740f-3804-4f93-a28f-a02e8f1cd06d', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('1423740f-3804-4f93-a28f-a02e8f1cd06d', magna_carta_1215__universal_rights_reading, influences).
narrative_ontology:cs_axiom('1423740f-3804-4f93-a28f-a02e8f1cd06d', foundational, precedential_accumulation_constitutes_legitimate_development).
narrative_ontology:cs_axiom_status(precedential_accumulation_constitutes_legitimate_development, holdable).
narrative_ontology:cs_axiom_grounding('1423740f-3804-4f93-a28f-a02e8f1cd06d', precedential_accumulation_constitutes_legitimate_development, conventional).
narrative_ontology:cs_axiom('1423740f-3804-4f93-a28f-a02e8f1cd06d', foundational, original_meaning_is_not_binding_on_present_interpretive_authority).
narrative_ontology:cs_axiom_status(original_meaning_is_not_binding_on_present_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('1423740f-3804-4f93-a28f-a02e8f1cd06d', original_meaning_is_not_binding_on_present_interpretive_authority, instrumental).
narrative_ontology:cs_reference_frame('1423740f-3804-4f93-a28f-a02e8f1cd06d', common_law_precedential_continuity_tradition).
narrative_ontology:cs_drift_state('1423740f-3804-4f93-a28f-a02e8f1cd06d', contemporary_originalism_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1423740f-3804-4f93-a28f-a02e8f1cd06d', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_reform_movements).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, modern_due_process_claimants).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, textual_originalist_litigants).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, precedential_accumulation_constitutes_constitutional_development).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, interpretive_authority_legitimately_supersedes_original_meaning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts across the common-law world treat Magna Carta as a living source they can reinterpret through accumulated precedent, extending or narrowing its reach (habeas corpus, due process, rule of law) without amendment. They administer the interpretive tradition and collect the authority that comes with being its custodians — every generation of judges gets to say what the document now means.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, common_law_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, common_law_judiciary, beneficiary).

% Advocacy groups and reformers invoke the living-document reading to argue that constitutional meaning can and should evolve to meet contemporary needs, using Magna Carta's own interpretive history as proof of concept that founding texts are not frozen. They benefit from a framework that legitimizes doctrinal change without requiring formal amendment.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_reform_movements, beneficiary,
    organized, generational, mobile, national).

% Individuals invoking due-process and rule-of-law protections traced genealogically to Clause 39 rely on centuries of precedential extension, not the 1215 text itself, to reach them. The living-document reading is what makes their claims cognizable at all — a strict originalist reading would exclude them as non-parties to a 13th-century baronial settlement.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, modern_due_process_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Litigants and scholars who want courts bound to demonstrable original meaning find their arguments structurally disadvantaged: the living-document framework treats fidelity to 1215 text as one interpretive option among several rather than a constraint on judicial authority. They bear the cost of an interpretive regime that can rule against textual fidelity claims by appeal to 'legitimate development.'
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, textual_originalist_litigants, payer,
    moderate, biographical, constrained, national).

% Historians who document what the 1215 charter actually settled — a feudal peace between King John and rebel barons — are frequently sidelined in doctrinal argument, where courts prioritize precedential lineage over historical accuracy. Their objection, that the living-document reading can launder discontinuity as continuity, rarely enters the courtroom record.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_historians, excluded,
    moderate, generational, analytical, national).

% Scholars of constitutional theory study how interpretive traditions accumulate authority over time and assess whether precedential development is legitimate constitutional change or judicial usurpation dressed in continuity. They take no side but supply the analytical vocabulary the contest is argued in.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__living_document_reading, common_law_judiciary).
narrative_ontology:fixing_cost_class(magna_carta_1215__living_document_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which a centuries-old founding text can remain a live source of legal authority without requiring either literal fidelity to its original 13th-century terms or formal re-ratification — coordinating successive generations of courts, litigants, and legislatures around a shared (if evolving) reference point.
% TRANSFER_FUNCTION: Moves interpretive authority from the original parties and their historically fixed meaning to the present judiciary and the precedential chain it administers; moves standing to claim constitutional protection from the narrow class of 1215 free men to whomever later precedent has judicially incorporated.
% ABSENT_VOICES: Legal historians documenting the feudal specificity of the 1215 settlement are structurally sidelined by doctrinal argument that treats precedential lineage as self-legitimating; textual originalists object that 'legitimate development' is indistinguishable in practice from ad hoc doctrinal invention, but their objection rarely displaces sitting precedent.
% DISAPPEARANCE_RATIONALE: If courts abandoned the living-document framework overnight, vast tracts of due-process, habeas corpus, and rule-of-law doctrine that trace their authority through accumulated precedent rather than direct textual warrant would lose their genealogical anchor — reformers, judges, and modern claimants would need an entirely different legitimating story, and originalist challenges to precedent-based rights would gain immediate traction.
% FOUNDING_PROBLEM: How can a legal-political order treat an ancient, historically specific settlement as continuously authoritative across radically changed social and political conditions, without either freezing law to 13th-century feudal terms or discarding the founding text's authority altogether?
% FOUNDING_PROBLEM_CORROBORATION: Constitutional theorists outside the judiciary and outside reform movements corroborate that the problem of adapting founding-text authority across time remains a live and unresolved question in comparative constitutional theory; legal historians corroborate that the 1215 text itself settles nothing about its own future interpretive status, which is precisely the gap the living-document reading fills — its status as a genuine solution versus a legitimating gloss on judicial power remains actively contested outside the judiciary's own self-description.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).
:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.38) and suppression (0.32) are moderate, not low or high: the living-document reading does real coordination work (allowing a founding text to remain operative across eight centuries of radical social change) but also transfers authority and standing in ways that disadvantage a specific class of litigant (textual originalists) who cannot easily exit the doctrinal system they contest. Theater ratio rises over the interval (0.2 to 0.4) reflecting the increasing use of precedential-continuity language to legitimate what are, on close historical inspection, substantive doctrinal innovations — the 'development' framing does genuine theoretical work but has also become a rhetorical device deployed regardless of whether genuine continuity exists. Accessibility collapse is moderate (0.35): the originalist alternative remains conceptually available and is actively argued in courts and scholarship, it has simply lost most doctrinal battles. Resistance is real (0.45): originalist and historicist critique of living-constitutionalism is a substantial, ongoing scholarly and judicial project, not a marginal position.
 *
 * DIRECTIONALITY LOGIC:
 *   The common-law judiciary is the structural agenda-setter and incidental beneficiary — it administers the interpretive chain and its authority grows with every precedent it lays down, giving it arbitrage-grade exit from any single doctrinal challenge. Reform movements and modern due-process claimants are beneficiaries whose claims are constituted by, not merely permitted by, the living-document framework. Textual originalist litigants are the payers: their arguments are treated as one interpretive option among several rather than a binding constraint on judicial authority, and their exit options are constrained because they must argue within the same court system that has institutionally adopted the framework they contest. Legal historians are excluded rather than harmed directly — their objection is structurally available but rarely decisive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to keep an ancient founding text authoritative across changed conditions — remains genuinely live (constitutional orders continuously face this problem), which is why this reading does not classify as a piton or scaffold. But the framework's own self-description (legitimate development) is precisely the claim under contest: whether precedential accumulation IS constitutional development or is a legitimating gloss for judicial power is not settled by the framework declaring itself legitimate. The classification as rope (genuine coordination function, moderate extraction, alternatives still live and argued) reflects that the coordination story is not mere cover — but the rising theater ratio and moderate extraction mean this is not a costless or fully innocent rope either; it sits closer to the boundary with tangled_rope than a comfortable rope classification might suggest, which is exactly the kind of measurement the framework is built to surface rather than paper over.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    development_vs_invention_boundary,
    'Is precedential accumulation a legitimate mode of constitutional development, or is it judicial invention retroactively legitimated by the appearance of continuity with an ancient text?',
    'No empirical resolution exists in principle — this is a jurisprudential and political-theoretic dispute about the nature of constitutional authority itself, resolvable only by which theory of legal legitimacy an observer antecedently accepts (originalist, common-law constitutionalist, or living-constitutionalist).',
    'If precedential accumulation is genuine development, the living-document reading is a rope: real coordination, legitimate evolution, moderate and justified extraction from originalist positions. If it is invention laundered as continuity, the reading functions closer to a tangled_rope or snare: judicial power extracting legitimacy from a historical artifact it has substantively abandoned.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(development_vs_invention_boundary, conceptual, 'Whether precedential accumulation constitutes legitimate constitutional development or judicial invention.').

omega_variable(
    kernel_reading_coexistence,
    'This constraint is one reading (living_document_reading) of the contested Magna Carta kernel. The sibling readings — baronial_privilege_reading (feudal-contract, ''free men'' equals landowning barons) and universal_rights_reading (transhistorical rights precedent, ''free men'' equals all persons) — make different first-order historical/textual claims. This reading makes a second-order meta-claim about interpretive authority that is compatible with adopting EITHER first-order reading as a historical baseline while still authorizing its supersession.',
    'No single resolution exists; the kernel is genuinely contested and each reading is held by different judicial and scholarly communities as a live position. The living-document reading is structurally compatible with acknowledging the baronial-privilege reading as historically accurate (1215 meaning) while asserting that this original meaning has been legitimately superseded — this is why it coexists with, rather than forecloses, the baronial reading.',
    'If the living-document reading''s legitimacy is rejected, both other readings gain force by default: originalists would push toward the baronial-privilege reading as the only historically defensible interpretation, while advocates of universal rights would need to ground Clause 39''s universal application in something other than legitimate interpretive development (e.g., independent natural-rights argument).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'This reading''s structural relationship to its sibling readings within the Magna Carta kernel.').

omega_variable(
    theater_or_genuine_continuity,
    'Is the rising theater_ratio (courts increasingly invoking ''legitimate constitutional development'' language) evidence that the coordination function is degrading into pure legitimation rhetoric, or does it reflect the natural maturation of a genuinely evolving interpretive tradition that requires more elaborate justification as it accumulates more precedent?',
    'Comparative analysis of cases where courts invoke living-document reasoning to reach outcomes inconsistent with prior precedent (theater) versus cases where the invocation tracks genuine, traceable doctrinal continuity (function) — a close reading of citation patterns across the measured interval.',
    'High theater with low genuine continuity would push this reading toward tangled_rope or piton; if the doctrinal continuity is robustly traceable, the rope classification with moderate extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_or_genuine_continuity, empirical, 'Whether rising invocation of ''legitimate development'' language tracks genuine or merely rhetorical continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__living_document_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t150, magna_carta_1215__living_document_reading, theater_ratio, 150, 0.25).
narrative_ontology:measurement_basis(magn_tr_t150, observed).
narrative_ontology:measurement(magn_tr_t300, magna_carta_1215__living_document_reading, theater_ratio, 300, 0.3).
narrative_ontology:measurement_basis(magn_tr_t300, observed).
narrative_ontology:measurement(magn_tr_t450, magna_carta_1215__living_document_reading, theater_ratio, 450, 0.33).
narrative_ontology:measurement_basis(magn_tr_t450, observed).
narrative_ontology:measurement(magn_tr_t600, magna_carta_1215__living_document_reading, theater_ratio, 600, 0.36).
narrative_ontology:measurement_basis(magn_tr_t600, observed).
narrative_ontology:measurement(magn_tr_t700, magna_carta_1215__living_document_reading, theater_ratio, 700, 0.38).
narrative_ontology:measurement_basis(magn_tr_t700, observed).
narrative_ontology:measurement(magn_tr_t800, magna_carta_1215__living_document_reading, theater_ratio, 800, 0.4).
narrative_ontology:measurement_basis(magn_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__living_document_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t150, magna_carta_1215__living_document_reading, base_extractiveness, 150, 0.2).
narrative_ontology:measurement_basis(magn_be_t150, observed).
narrative_ontology:measurement(magn_be_t300, magna_carta_1215__living_document_reading, base_extractiveness, 300, 0.25).
narrative_ontology:measurement_basis(magn_be_t300, observed).
narrative_ontology:measurement(magn_be_t450, magna_carta_1215__living_document_reading, base_extractiveness, 450, 0.3).
narrative_ontology:measurement_basis(magn_be_t450, observed).
narrative_ontology:measurement(magn_be_t600, magna_carta_1215__living_document_reading, base_extractiveness, 600, 0.34).
narrative_ontology:measurement_basis(magn_be_t600, observed).
narrative_ontology:measurement(magn_be_t700, magna_carta_1215__living_document_reading, base_extractiveness, 700, 0.36).
narrative_ontology:measurement_basis(magn_be_t700, observed).
narrative_ontology:measurement(magn_be_t800, magna_carta_1215__living_document_reading, base_extractiveness, 800, 0.38).
narrative_ontology:measurement_basis(magn_be_t800, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_1215__living_document_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__living_document_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'the Magna Carta' per the epsilon-invariance principle. baronial_privilege_reading treats the 1215 text as a closed feudal contract with a fixed, narrow beneficiary class and low ongoing extraction (a near-mountain historical fact). universal_rights_reading treats Clause 39 as always having emitted a transhistorical universal due-process guarantee, with its own distinct epsilon and victim/beneficiary structure. This story (living_document_reading) makes neither first-order historical claim; it is a meta-constraint about the legitimacy of interpretive authority itself, structurally compatible with either first-order reading as a historical baseline. Its epsilon (0.38) reflects the moderate extraction of transferring authority to the judiciary and disadvantaging textual-fidelity claims — distinct from and not reducible to either sibling's epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
