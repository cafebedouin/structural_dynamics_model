% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage Sacrament: Civic Pastoral Reading
 *   domain: religious/doctrinal/political
 *
 * SUMMARY:
 *   This constraint instantiates the CIVIC PASTORAL READING of the marriage
 *   sacrament kernel. It models marriage as a pastoral relationship
 *   inherently subject to human failure, where indissolubility functions as
 *   an aspirational ideal rather than an ontologically constitutive reality
 *   requiring hierarchical adjudication. Under this reading, pastoral bishops
 *   possess delegated authority to exercise compassionate discernment in
 *   individual cases — annulments are granted more readily, remarriage is
 *   less categorically prohibited, and divorced Catholics gain expanded
 *   access to sacramental life. The reading creates moderate extractiveness
 *   because it benefits divorced-remarried Catholics and pastoral bishops at
 *   the cost of doctrinal relativization that harms traditional Catholics
 *   whose identity depends on stable doctrine, and at the cost of
 *   institutional recognition for abandoned spouses whose marriages are
 *   annulled retroactively. The competing HIERARCHICAL INDISSOLUBILITY
 *   READING (constraint_id:
 *   marriage_sacrament__hierarchical_indissolubility_reading) models marriage
 *   as an ontological reality whose indissolubility is constitutive, not
 *   aspirational; under that reading, the pastoral reading appears as
 *   doctrinal corruption. The two readings coexist across different diocesan
 *   and national jurisdictions, neither fully foreclosing the other within
 *   the universal Church's framework, though each influences the legitimacy
 *   conditions of the other.
 *
 * KEY AGENTS:
 *   - pastoral_bishops: institutional agenda-setters applying compassionate discretion in annulment proceedings and moral guidance
 *   - divorced_remarried_catholics: organized beneficiaries gaining sacramental access and moral validation
 *   - traditional_indissolubility_catholics: moderate-power victims experiencing doctrinal relativization and normative uncertainty; identity_locked (their Catholicism is constituted through adherence to stable doctrine)
 *   - abandoned_spouses: moderate-power victims whose marriages are retroactively annulled; structurally excluded from annulment proceedings
 *   - vatican_magisterium: institutional observer that created the doctrinal ambiguity this reading exploits
 *   - traditional_diocesan_authorities: institutional observers maintaining the hierarchical reading in their own jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.58).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.47).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage Sacrament: Civic Pastoral Reading").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious/doctrinal/political").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '21faf02a-39cd-41cc-83bb-527d2c3bce9e').
narrative_ontology:cs_kernel_codification('21faf02a-39cd-41cc-83bb-527d2c3bce9e', formalized).
narrative_ontology:cs_authority_grounding('21faf02a-39cd-41cc-83bb-527d2c3bce9e', lineage).
narrative_ontology:cs_interpretation_layer_present('21faf02a-39cd-41cc-83bb-527d2c3bce9e').
narrative_ontology:cs_reading_relation('21faf02a-39cd-41cc-83bb-527d2c3bce9e', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('21faf02a-39cd-41cc-83bb-527d2c3bce9e', foundational, indissolubility_aspirational_not_constitutive).
narrative_ontology:cs_axiom_status(indissolubility_aspirational_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('21faf02a-39cd-41cc-83bb-527d2c3bce9e', indissolubility_aspirational_not_constitutive, deontological).
narrative_ontology:cs_axiom('21faf02a-39cd-41cc-83bb-527d2c3bce9e', foundational, pastoral_mercy_over_doctrinal_clarity).
narrative_ontology:cs_axiom_status(pastoral_mercy_over_doctrinal_clarity, holdable).
narrative_ontology:cs_axiom_grounding('21faf02a-39cd-41cc-83bb-527d2c3bce9e', pastoral_mercy_over_doctrinal_clarity, deontological).
narrative_ontology:cs_reference_frame('21faf02a-39cd-41cc-83bb-527d2c3bce9e', post_vatican_ii_pastoral_mercy).
narrative_ontology:cs_drift_state('21faf02a-39cd-41cc-83bb-527d2c3bce9e', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21faf02a-39cd-41cc-83bb-527d2c3bce9e', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_bishops).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_indissolubility_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, abandoned_spouses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diocesan bishops in ecclesial regions that interpret marriage doctrine through the pastoral lens: indissolubility is an ideal requiring compassionate case-by-case discernment rather than categorical enforcement. They exercise administrative discretion in annulment proceedings and moral guidance to remarried divorcees, applying doctrinal relativization to reduce pastoral harm. Their authority to set local interpretation derives from post-Vatican II delegated discretion and magisterial ambiguity about binding force of indissolubility doctrine.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_bishops, agenda_setter,
    institutional, generational, constrained, national).

% Receive expanded pastoral access and moral validation under this reading: their remarriage is no longer categorically treated as adultery; annulments are granted with higher frequency and looser evidentiary standards; Eucharistic reception and sacramental participation become possible through pastoral discretion rather than canonical restriction. The constraint gives them a path to remain Catholic while remaining remarried.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics, beneficiary,
    organized, biographical, constrained, national).

% Experience doctrinal relativization and normative uncertainty: the stable, binding force of the indissolubility teaching — which structured their identity, formed their marital commitment, and provided clear guidance for moral life — has become negotiable and subject to pastoral discretion. They bear the cost of doctrinal inconsistency: the doctrine they were taught as binding is applied selectively; their fidelity to the stricter interpretation becomes personally costly (social standing among peers who have obtained annulments; perceived as rigid or uncharitable). Their identity is constituted through adherence to the stability of doctrine; the reading's operation destabilizes that identity.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_indissolubility_catholics, payer,
    moderate, biographical, identity_locked, national).

% Experience erasure: when a marriage is annulled under pastoral discretion, the abandoned spouse's claims to have been validly married are retroactively negated. Under the hierarchical reading, annulment required rigorous proof of impediment; under the pastoral reading, the process becomes faster, more forgiving of the remarrying party, and leaves the innocent party without institutional recognition of the harm done. They are structurally excluded from the renegotiation: they do not participate in annulment proceedings and cannot block the declaration of nullity.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, abandoned_spouses, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, abandoned_spouses, excluded).

% Central teaching authority that created the doctrinal ambiguity the pastoral reading exploits: post-Vatican II language about mercy, pastoral discernment, and the possibility of interior consent failure left indissolubility stated but not clarified. Rome observes but has not formally reversed the doctrine; the pastoral reading operates in the space of that unresolved tension. The magisterium is both the author of the ambiguity and the observer of its effects.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, vatican_magisterium, observer,
    institutional, civilizational, analytical, universal).

% Diocesan authorities that maintain the hierarchical reading: they continue to apply stricter annulment standards and maintain that indissolubility is binding doctrine, not an ideal subject to discretion. They are not parties to the pastoral reading but exist in tension with it, creating visible divergence in pastoral practice across diocesan lines.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_diocesan_authorities, observer,
    institutional, generational, analytical, national).

% Organized groups of Catholic spouses abandoned after valid marriages, who oppose pastoral discretion annulments on grounds that they erase the truth of the marriage and leave the innocent party without institutional recourse. They would argue for the hierarchical reading but are structurally excluded from diocesan decision-making processes about annulments and pastoral norms.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, faithful_abandoned_spouses_networks, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__civic_pastoral_reading, pastoral_bishops).
narrative_ontology:fixing_cost_class(marriage_sacrament__civic_pastoral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine pastoral problem: rigid enforcement of indissolubility doctrine in cases of irreparable marriage breakdown causes spiritual harm and drives people from the Church. The pastoral reading coordinates compassionate response with doctrinal continuity by reinterpreting indissolubility as an aspiration rather than an absolute — it permits the Church to remain unified while extending mercy to those whose marriages have failed.
% TRANSFER_FUNCTION: Transfers doctrinal stability and normative certainty from traditional Catholics who depend on binding doctrine to divorced-remarried Catholics who gain expanded sacramental access and moral validation. The constraint moves legitimacy from strict interpretation to compassionate discretion; it moves sacramental access from the strictly bound to the pastorally situated.
% ABSENT_VOICES: Abandoned spouses who did not seek the divorce are structurally excluded from annulment proceedings and have no voice in determining whether their marriage is declared null. Their testimony, if heard, would center on the truth of the marriage and the injustice of retroactive negation. They are present but not authorized speakers in the discernment process.
% DISAPPEARANCE_RATIONALE: If pastoral discretion annulments and the pastoral reading vanished overnight, Catholic practice around divorce would revert to stricter enforcement: remarried divorcees would lose sacramental access or be restored to the status quo ante this reading created; the Church would present a unified hierarchical doctrine on indissolubility; traditional Catholics would recover doctrinal stability; abandoned spouses would regain institutional recognition (hard to annul under strict standards). The pastoral reading is not merely an interpretation — it is a functional constraint that reorganizes who has standing, what is licit, and what the doctrine means.
% FOUNDING_PROBLEM: The Church's pastoral crisis of the second half of the twentieth century: widespread marriage breakdown in Western Catholicism, the experience of rigid indissolubility doctrine as causing spiritual harm and driving people from the faith, and the conflict between magisterial insistence on indissolubility and lived experience of marriage failure. Vatican II opened language about mercy and pastoral discernment; bishops in pastoral settings found themselves unable to apply strict doctrine without causing what they experienced as spiritual abuse.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral bishops and divorced-remarried Catholics attest the founding problem remains live and justify the pastoral reading as a compassionate response. Traditional Catholics and canonical scholars attest the founding problem has been reframed rather than solved — the constraint has substituted pastoral relativization for doctrinal clarity, leaving the Church teaching one doctrine and practicing another. Theological historians outside both camps document that Vatican II's language created genuine ambiguity rather than resolving the underlying tension.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.32 to 0.58 over the interval, then plateaus. Early extractiveness is low because the pastoral reading is still emerging as a distinct practice (t=0, Vatican II aftermath, practice not yet systemized). Extractiveness rises sharply through t=0-32 as pastoral discretion becomes institutionalized in practices — more annulments granted, looser evidentiary standards, expanded moral permission for remarriage — and the cost to traditional Catholics accumulates (doctrinal relativization is experienced as a loss). At t=32-50, extractiveness plateaus as the reading stabilizes into a steady-state practice: it has reorganized who benefits and who pays, but the rate of new extraction does not continue rising. Theater ratio rises similarly (28% → 52%): early pastoral discretion retains real function (genuinely responding to pastoral crisis), but by t=50, a significant share of activity is performative maintenance of doctrinal stability — bishops invoke the doctrine while routinely circumventing it, creating visible theatrical work to reconcile incompatible commitments. Suppression is stable (0.38 → 0.47): the constraint is maintained by institutional authority (the bishop's delegated discretion) rather than by coercive force against external actors, but suppression grows slightly as the reading must suppress the contrary hierarchical interpretation and marginalize abandoned spouses from standing in annulment processes. The temporal pattern models extraction accumulation (T17 candidate) as institutional discretion metastasizes into systematic relativization.
 *
 * PERSPECTIVAL GAP:
 *   The pastoral_bishops seat computes this as genuine coordination solving a real pastoral crisis — they genuinely experience themselves as solving a coordination problem between doctrine and lived experience. The divorced_remarried_catholics seat experiences it as expansion of legitimate options and moral standing — a beneficent reclassification. The traditional_indissolubility_catholics seat computes this as a snare or degraded piton — they are coordinated into accepting doctrinal instability, and they bear the cost of that relativization through loss of normative clarity and identity destabilization. The abandoned_spouses seat computes this as a snare with exclusion — they have no standing in the very process (annulment) that erases their marriage; they are not agents in the coordination but are casualties of it. The engine should compute different types for each seat from the structural data: pastoral_bishops as beneficiaries with low d, traditional_indissolubility_catholics as victims with high d and identity_locked exit (their exit is not really available — leaving Catholicism is not a live option when their identity is constituted through Catholic doctrine), divorced_remarried_catholics as moderate d (they benefit but also remain subject to the constraint of pastoral discretion), and abandoned_spouses as victims with high d.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: pastoral_bishops (d ≈ 0.2-0.3) benefit from the discretion granted them and the institutional stability this reading provides; the constraint enhances their power. Divorced_remarried_catholics (d ≈ 0.35-0.45) benefit from expanded access but remain subject to pastoral discretion — their exit options are constrained; they cannot unilaterally obtain annulments but must receive pastoral approval; their benefit is real but mediated through the agenda-setter's judgment. Victim directionality: traditional_indissolubility_catholics (d ≈ 0.75-0.85) experience high extraction because (1) the doctrine they were taught as binding is applied selectively, (2) they face social costs for adhering to the stricter interpretation, and (3) their identity is destabilized by the normative uncertainty the reading creates. Critically, their exit_options are identity_locked — they cannot leave Catholicism without dissolving their self-concept; they cannot exit the constraint by leaving the Church; they must absorb the extraction. Abandoned_spouses (d ≈ 0.70-0.80) experience extraction through exclusion and erasure — their standing in the constraint's application is zero; the constraint operates on their marriage without their participation. No directionality override is needed: the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by declaring it tangled_rope rather than rope or snare. If labeled rope, it would appear as pure coordination with beneficiaries balancing costs — false, because traditional Catholics experience uncompensated loss and abandoned spouses are uncompensated for erasure. If labeled snare, it would appear as pure extraction with identifiable victims — partially true for traditional Catholics and abandoned spouses, but the constraint genuinely solves a coordination problem (pastoral crisis) that the hierarchical reading does not solve. Tangled_rope is correct: it coordinates a real solution (pastoral response to marriage failure) AND extracts asymmetrically from those who depend on doctrinal stability and from those whose marriages are annulled retroactively. The mandatrophy resolution is that indissolubility was FOUNDED to solve a problem (the need for marital stability and indissolubility as a structural good) that remains real but is being reinterpreted; the pastoral reading is not maintaining the original mandate but redefining it toward pastoral mercy and away from doctrinal stability. A church member who joined the Church because of its binding doctrine on indissolubility has experienced mandate reversion: the basis of their commitment has shifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_identity_locked,
    'Is the suppression of traditional indissolubility doctrine experienced by traditional Catholics structural (they cannot exit the institutional constraint because of geographic or social dependency on the Church) or internalized (they have fused their identity with the doctrine and cannot conceptually exit even if institutional barriers were removed)?',
    'Ethnographic study of traditional Catholics'' reaction to doctrinal relativization after leaving organized Catholic practice; post-exit suppression trajectory. If suppression persists after institutional disengagement, the mechanism is partly internalized.',
    'If suppression is internalized, the constraint''s effective extractiveness is higher than measured — the target carries the suppression with them after exit. If structural, doctrinal reforms or institutional exits could reduce it. This determines whether traditional Catholics are truly trapped or only constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_locked, empirical, 'Structural vs. internalized suppression in identity-locked victims of doctrinal relativization').

omega_variable(
    doctrinal_ambiguity_authorship,
    'Did Vatican II intentionally create the doctrinal ambiguity that the pastoral reading exploits, or did the pastoral reading unintentionally emerge from ambiguous post-Vatican II language? Is the ambiguity a design choice or a textual accident?',
    'Close reading of Vatican II primary sources and interviews with its principal drafters (where available from archives); comparison with clear doctrinal statements in other domains.',
    'If intentional design, the pastoral reading is a foreseen development; if accidental, it is an uncontrolled interpretation that the magisterium must either formalize or foreclose. This determines whether the pastoral reading has magisterial legitimacy or is operating in a zone of permitted ambiguity without explicit endorsement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_ambiguity_authorship, conceptual, 'The authorship and intentionality of the doctrinal ambiguity structuring this reading').

omega_variable(
    annulment_evidentiary_standard_drift,
    'Over the interval t=0-50, did the evidentiary standard for granting annulments in pastoral dioceses actually decline (more annulments granted on weaker evidence), or did the APPEARANCE of decline arise because the same absolute standard was applied to a higher volume of cases, making approval more visible?',
    'Quantitative analysis of annulment proceedings in pastoral dioceses: comparison of approved vs. denied cases, evidence quality scores, ratios of approvals over time.',
    'If the standard declined, the pastoral reading is systematically relativizing doctrine. If the standard is stable but volume increased, the reading redistributes access without lowering standards — a different structural story. This determines whether the constraint extracts through doctrinal corruption or through equitable volume redistribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(annulment_evidentiary_standard_drift, empirical, 'The historical trajectory of annulment evidentiary standards in pastoral dioceses').

omega_variable(
    abandoned_spouse_institutional_erosion,
    'Did the pastoral reading''s expansion of annulments retroactively erase marriages of abandoned spouses who remain in the Church? Or did the reading''s application create a secondary norm where annulments are granted only when BOTH parties consent or only when the seeking party is the one who suffered abandonment?',
    'Case-law review of annulments granted to abandoned remarrying parties; documentation of institutional norms about whether innocent abandoned spouses retained standing to object or prevent annulment.',
    'If both parties'' consent is required, abandoned spouses retain some standing and the extraction is partial. If the seeking party can unilaterally obtain annulment, the extraction is nearly complete (the innocent spouse''s marriage is erased without their participation). This determines the severity of the victim status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abandoned_spouse_institutional_erosion, empirical, 'The degree of institutional standing retained by abandoned spouses in annulment proceedings under the pastoral reading').

omega_variable(
    kernel_contest_foreclosure_possibility,
    'Can the pastoral reading and the hierarchical indissolubility reading coexist indefinitely within the universal Church''s single framework, or does one necessarily foreclose the other at some point? Is the coexistence a stable equilibrium or a temporary state destined to resolve into foreclosure?',
    'Doctrinal analysis of whether the two readings'' core axioms (indissolubility as aspirational vs. constitutive) can logically coexist in a single teaching framework. If not, doctrinal rupture will eventually occur and one reading will be formally rejected or modified.',
    'If foreclosure is inevitable, the pastoral reading is unstable and will either be formally endorsed (becoming the new universal doctrine) or formally rejected (reverting to hierarchical). The constraint''s terminal type depends on which resolution occurs. If coexistence is stable, the constraint persists as tangled_rope indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_possibility, conceptual, 'The logical stability of concurrent pastoral and hierarchical indissolubility readings in a single Church framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(marr_tr_t8, marriage_sacrament__civic_pastoral_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(marr_tr_t16, marriage_sacrament__civic_pastoral_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(marr_tr_t24, marriage_sacrament__civic_pastoral_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(marr_tr_t32, marriage_sacrament__civic_pastoral_reading, theater_ratio, 32, 0.54).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__civic_pastoral_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__civic_pastoral_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(marr_be_t8, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(marr_be_t16, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(marr_be_t24, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(marr_be_t32, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(marr_su_t8, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(marr_su_t16, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(marr_su_t24, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(marr_su_t32, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 50, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__civic_pastoral_reading, 0.12).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% The marriage sacrament kernel decomposes into two structurally distinct constraints: (1) the CIVIC_PASTORAL_READING (this file), modeling indissolubility as aspirational and subject to pastoral discretion, with moderate extractiveness arising from doctrinal relativization; (2) the HIERARCHICAL_INDISSOLUBILITY_READING (sibling file), modeling indissolubility as ontologically constitutive and binding, with lower extractiveness for beneficiaries of strict doctrine and higher extractiveness for those who experience the doctrine as rigid. These are not the same constraint viewed from two angles — their ε values differ significantly, their beneficiary/victim structures diverge, and their terminal classifications differ. The confusion was in the natural-language label ('marriage indissolubility doctrine') which masks two structurally distinct claims. This reading INFLUENCES the hierarchical reading by creating legitimacy pressure and enforcement visibility; the hierarchical reading influences this reading by providing doctrinal counterpressure. They coexist across diocesan boundaries and represent an unresolved kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__civic_pastoral_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
