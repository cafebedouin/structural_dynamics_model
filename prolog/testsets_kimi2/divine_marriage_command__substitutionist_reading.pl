% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Post-Manifesto Monogamy Doctrine â Substitutionist Reading
 *   domain: religious/political/theological
 *
 * SUMMARY:
 *   The divine_marriage_command kernel governs the theological status of
 *   plural marriage in a restorationist Christian tradition. The
 *   substitutionist reading treats the 1890 Manifesto as a genuine
 *   superseding revelation that permanently rescinds the prior divine command
 *   to practice polygamy, making post-Manifesto plural marriage apostasy.
 *   This constraint is actively enforced through excommunication,
 *   institutional boundary maintenance, and the suppression of the
 *   alternative coercion-and-survival narrative. The reading instantiates a
 *   tangled rope: it coordinates the orthodox community around a unified
 *   monogamous marriage standard while extracting from fundamentalist
 *   practitioners through excommunication and social death. The structural
 *   delta includes the redefinition of polygamy from sacred duty to apostasy,
 *   the institutional necessity of framing the shift as revelation rather
 *   than federal accommodation, and the disciplinary exclusion of
 *   continuationist fundamentalists. This reading forecloses the
 *   continuationist reading within the institutional framework and influences
 *   the coercion_visibility reading by rendering it theologically
 *   illegitimate.
 *
 * KEY AGENTS:
 *   - church_hierarchy: Primary agenda-setter (institutional/identity_locked/global) â administers the Manifesto as revelation, enforces excommunication, and captures institutional legitimacy.
 *   - orthodox_membership: Primary beneficiary (organized/constrained/global) â receives doctrinal clarity and community boundaries, sustains the hierarchy.
 *   - fundamentalist_practitioners: Primary payer (powerless/trapped/regional) â bears excommunication, loss of family and sealing status, and criminalization.
 *   - secular_federal_government: Excluded actor (institutional/analytical/national) â the coercive pressure it applied is officially erased from the theological narrative.
 *   - scholarly_historians: Analytical observer (analytical/analytical/national) â documents the divergence between official revelation-claims and archival evidence of political accommodation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.74).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.8).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Post-Manifesto Monogamy Doctrine â Substitutionist Reading").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious/political/theological").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, 'b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98').
narrative_ontology:cs_kernel_codification('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', fixed_text).
narrative_ontology:cs_authority_grounding('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', lineage).
narrative_ontology:cs_interpretation_layer_present('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98').
narrative_ontology:cs_reading_relation('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', foundational, supersession_revelation_doctrine).
narrative_ontology:cs_axiom_status(supersession_revelation_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', supersession_revelation_doctrine, theological).
narrative_ontology:cs_axiom('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', foundational, monogamy_as_current_divine_standard).
narrative_ontology:cs_axiom_status(monogamy_as_current_divine_standard, holdable).
narrative_ontology:cs_axiom_grounding('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', monogamy_as_current_divine_standard, theological).
narrative_ontology:cs_reference_frame('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', prophetic_continuity_framework).
narrative_ontology:cs_drift_state('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b5cf54cf-088f-4d2b-8f60-7a6f07e5ff98', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, orthodox_membership).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_practitioners).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, prophetic_supersession_principle).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, monogamy_as_divine_ordinance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrinal boundary between orthodox monogamy and apostate polygamy. Issues the Manifesto as a superseding revelation, disciplines post-Manifesto plural marriage through excommunication, and derives institutional legitimacy from the framing that this shift is divine course-correction rather than political accommodation. Captures consolidated authority and eliminates a practice that threatened federal incorporation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, church_hierarchy, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive doctrinal clarity and community boundaries that distinguish legitimate monogamous families from apostate practice. Their orthodox status is reaffirmed by the institutional framing of the Manifesto as revelation. They sustain the hierarchy through tithes, obedience, and social enforcement of the new norm, while paying the diffuse cost of foreclosed theological questioning.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, orthodox_membership, beneficiary,
    organized, generational, constrained, global).

% Continue to regard polygamy as doctrinally valid and practice it in defiance of the Manifesto. Bear the costs of excommunication, loss of temple access, social death within their families, and legal prosecution. Their theological reading is structurally excluded from institutional discourse; they are defined as apostates rather than dissenters.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamentalist_practitioners, payer,
    powerless, biographical, trapped, regional).

% Applied federal pressure that precipitated the Manifesto, but under the substitutionist reading this causal role is theologically invisible. The official narrative excludes state coercion as the relevant explanation; the government is not in the room where the revelation is canonized.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, secular_federal_government, excluded,
    institutional, generational, analytical, national).

% Document the historical sequence of federal pressure, political negotiation, and theological reframing. They observe the divergence between the institutional revelation-claim and the archival record of coercion, without being bound by the doctrinal commitment.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, scholarly_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates marriage and family formation within the religious community around a single, universally applicable monogamous standard, resolving disputes over legitimacy, inheritance, and membership status by anchoring them to a clear doctrinal boundary administered by living prophets.
% TRANSFER_FUNCTION: Moves theological legitimacy, community membership, and family integrity from polygamous practitioners to the monogamous orthodox body and the institutional hierarchy that defines the boundary, via the mechanism of excommunication and apostasy designation.
% ABSENT_VOICES: Fundamentalist practitioners who still regard polygamy as doctrinally valid are structurally excluded from institutional discourse â their testimony is defined as apostasy rather than dissent. The federal government's coercive role is theologically invisible. Post-Manifesto polygamous families are not in the room where the revelation is received and canonized.
% DISAPPEARANCE_RATIONALE: If the substitutionist constraint vanished â if the Manifesto were no longer treated as superseding revelation â the institutional boundary between orthodox monogamy and apostasy would dissolve. Polygamous practitioners would re-enter legitimate discourse, the hierarchy would lose its post-Manifesto doctrinal anchor, and the community would split over whether to revert to prior command or adopt prudential suspension. The social and theological map of the community would rearrange.
% FOUNDING_PROBLEM: The existential crisis of maintaining a divinely commanded practice (polygamy) under federal coercion that threatened institutional dissolution, requiring a theological mechanism to abandon the practice without abandoning prophetic authority or admitting error.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and the coercion_visibility_reading attest that the founding problem was federal survival. The substitutionist reading itself does not claim the problem is still live â it treats the Manifesto as settled revelation. Corroboration from outside the benefiting parties: federal court records, congressional legislation (Edmunds-Tucker Act), and post-Manifesto memoirs from non-beneficiary parties document the coercion. No corroboration exists from outside the church that the problem was a purely theological need for updated revelation; that attestation comes only from the beneficiary set.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74) is high because the constraint strips fundamentalist practitioners of community, family, and soteriological standing â a severe transfer masked as doctrinal boundary maintenance. Suppression (0.80) is high because the constraint's persistence requires active excommunication machinery, suppression of the coercion narrative, and the delegitimation of continuationist theology. Theater_ratio (0.45) reflects significant performative maintenance: the revelation framing must be continuously rehearsed in institutional discourse to obscure the political accommodation that produced the Manifesto. Accessibility_collapse (0.75) is high for fundamentalists (exiting the constraint means abandoning their entire theological and kinship world) and moderate for orthodox members (leaving is socially costly but structurally possible). Resistance (0.60) reflects persistent underground polygamous practice, splinter sect formation, and modern historical scholarship that recovers the coercion narrative. Temporal measurements show extraction and theater rising as the original political context faded from institutional memory, then stabilizing as the substitutionist narrative became fully naturalized.
 *
 * PERSPECTIVAL GAP:
 *   The church_hierarchy seat experiences the constraint as legitimate doctrinal coordination â a necessary update from a living prophet. The fundamentalist_practitioners seat experiences the same structure as violent extraction of their families, salvation, and theological identity. The orthodox_membership seat sits nearer symmetric: they benefit from clear norms but also pay through restricted doctrinal questioning and the social costs of enforcing exclusion. The engine computes these divergences from the structural data; the authored claim (tangled_rope) does not resolve the dispute but names the hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the church_hierarchy (institutional legitimacy, consolidated authority, elimination of a politically fatal practice) and orthodox_membership (clear membership boundaries, reaffirmed prophetic confidence). The victim/payer is fundamentalist_practitioners, who bear excommunication and social death. Directionality derived from these declarations places the hierarchy and orthodox membership near the beneficiary pole (low d, low or negative effective extraction) and fundamentalists near the full-target pole (high d, amplified chi). No override is needed: the structural derivation captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â federal dissolution of the church â is dead. The constraint persists over a century after the Edmunds-Tucker Act ceased to threaten institutional survival. The R5 genealogy (dead founding problem plus world_rearranges disappearance verdict) flags a zombie or capture dynamic: the constraint has outlived its original function and now operates as boundary maintenance. However, the constraint is not a pure snare because the coordination function (unified marriage standard, community coherence) remains live for the orthodox body. The tangled_rope classification captures this hybrid: genuine coordination layered atop extraction from the excluded fundamentalist population. Without mandatrophy analysis, the constraint could be misread as either a legitimate scaffold (it is not transitional â no sunset) or a pure snare (it does coordinate the orthodox community).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_sincerity,
    'Was the Manifesto issued and received as a genuine divine revelation superseding prior command, or as a politically necessary framing to preserve institutional survival under federal coercion?',
    'Archival discovery of internal church correspondence around 1890; analysis of the chronological gap between federal pressure peaks and the revelation claim; comparative study of how the institution treated pre-Manifesto polygamy versus post-Manifesto polygamy.',
    'If the revelation was sincere and primary, the constraint''s high extraction from fundamentalists is the cost of maintaining divine order; if it was primarily political framing, the constraint is a snare using theological language to enforce a secular settlement, and the theater_ratio would be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_sincerity, empirical, 'Whether the Manifesto was genuine revelation or political accommodation').

omega_variable(
    apostasy_definition_power,
    'Does the power to define post-Manifesto polygamy as apostasy rest on theological substance or on institutional control of the membership narrative?',
    'Cross-reading comparison: if the continuationist reading can produce a coherent theological exegesis without institutional rupture, the apostasy definition is institutionally contingent rather than theologically necessary.',
    'If institutionally contingent, the extraction from fundamentalist practitioners is structurally identical to disciplinary exclusion for organizational compliance rather than heresy, pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apostasy_definition_power, conceptual, 'Whether apostasy designation is theological or institutional').

omega_variable(
    cs_framing_alternative,
    'Is the relevant commitment system the formalized text of the Manifesto itself, or the interpretive tradition that layers continuous revelation above the text to manage doctrinal change?',
    'Examination of which layer changes when future doctrinal reversals occur: does the text change, or does the interpretive framework absorb the reversal without text revision?',
    'If the text is the kernel, the authority_grounding is lineage tied to a fixed document; if the interpretive tradition is the kernel, the authority_grounding is practice and the constraint''s CS pattern shifts from formalized to implicit, altering drift_state direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_alternative, conceptual, 'Alternative CS framing of kernel location').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__substitutionist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(divi_tr_t60, divine_marriage_command__substitutionist_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(divi_tr_t80, divine_marriage_command__substitutionist_reading, theater_ratio, 80, 0.5).
narrative_ontology:measurement(divi_tr_t100, divine_marriage_command__substitutionist_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(divi_tr_t130, divine_marriage_command__substitutionist_reading, theater_ratio, 130, 0.45).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__substitutionist_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(divi_be_t60, divine_marriage_command__substitutionist_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement(divi_be_t80, divine_marriage_command__substitutionist_reading, base_extractiveness, 80, 0.72).
narrative_ontology:measurement(divi_be_t100, divine_marriage_command__substitutionist_reading, base_extractiveness, 100, 0.73).
narrative_ontology:measurement(divi_be_t130, divine_marriage_command__substitutionist_reading, base_extractiveness, 130, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__substitutionist_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(divi_su_t60, divine_marriage_command__substitutionist_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(divi_su_t80, divine_marriage_command__substitutionist_reading, suppression_requirement, 80, 0.78).
narrative_ontology:measurement(divi_su_t100, divine_marriage_command__substitutionist_reading, suppression_requirement, 100, 0.79).
narrative_ontology:measurement(divi_su_t130, divine_marriage_command__substitutionist_reading, suppression_requirement, 130, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the divine_marriage_command kernel. It is distinguished from the continuationist reading by whether polygamy remains doctrinally valid, and from the coercion_visibility reading by whether the Manifesto's legitimacy derives from divine revelation or institutional survival.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
