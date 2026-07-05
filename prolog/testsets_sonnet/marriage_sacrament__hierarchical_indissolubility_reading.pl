% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Sacramental Marriage as Ontologically Indissoluble Bond Requiring Tribunal Adjudication
 *   domain: religious doctrine / canon law / political sociology
 *
 * SUMMARY:
 *   This constraint isolates one structurally distinct reading within the
 *   contested marriage_sacrament kernel: the
 *   hierarchical_indissolubility_reading holds that sacramental marriage is
 *   an ontological reality, that indissolubility is constitutive rather than
 *   aspirational, and that determination of marital validity therefore
 *   requires formal hierarchical adjudication (the diocesan tribunal system)
 *   rather than local pastoral discernment. This is NOT the same constraint
 *   as the sibling civic_pastoral_reading, which treats indissolubility as an
 *   ideal subject to compassionate case-by-case discernment — that reading
 *   has its own, lower-extraction structural profile and is authored as a
 *   separate story. Under this reading, the tribunal apparatus, the annulment
 *   fee and delay structure, and the denial of Communion to
 *   divorced-and-remarried Catholics who have not obtained a declaration of
 *   nullity are read as necessary consequences of the ontological claim, not
 *   as optional pastoral policy. The expected structural delta (high
 *   extractiveness via sacramental exclusion, tribunal cost/delay burden, and
 *   institutional enforcement) is exactly what this reading's metrics encode.
 *
 * KEY AGENTS:
 *   - magisterial_teaching_authority: sets doctrine (institutional/arbitrage) — defines the ontological premise
 *   - diocesan_tribunal_system: administers adjudication (institutional/arbitrage) — collects fees, imposes delay, gates sacramental access
 *   - divorced_remarried_catholics: bears exclusion (powerless/trapped) — denied Communion absent tribunal validation
 *   - canon_law_professional_class: professional beneficiary (organized/constrained) — livelihood tied to the adjudication requirement
 *   - canon_law_scholars_and_sociologists_of_religion: analytical observer (analytical) — documents disparate access and impact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.68).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.71).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Sacramental Marriage as Ontologically Indissoluble Bond Requiring Tribunal Adjudication").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious doctrine / canon law / political sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '6a9dffa4-9d2e-4c61-92c2-089acb6ab779').
narrative_ontology:cs_kernel_codification('6a9dffa4-9d2e-4c61-92c2-089acb6ab779', formalized).
narrative_ontology:cs_authority_grounding('6a9dffa4-9d2e-4c61-92c2-089acb6ab779', lineage).
narrative_ontology:cs_interpretation_layer_present('6a9dffa4-9d2e-4c61-92c2-089acb6ab779').
narrative_ontology:cs_reading_relation('6a9dffa4-9d2e-4c61-92c2-089acb6ab779', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('6a9dffa4-9d2e-4c61-92c2-089acb6ab779', foundational, marriage_bond_is_ontologically_indissoluble).
narrative_ontology:cs_axiom_status(marriage_bond_is_ontologically_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('6a9dffa4-9d2e-4c61-92c2-089acb6ab779', marriage_bond_is_ontologically_indissoluble, deontological).
narrative_ontology:cs_axiom('6a9dffa4-9d2e-4c61-92c2-089acb6ab779', foundational, validity_determination_requires_hierarchical_juridical_process).
narrative_ontology:cs_axiom_status(validity_determination_requires_hierarchical_juridical_process, holdable).
narrative_ontology:cs_axiom_grounding('6a9dffa4-9d2e-4c61-92c2-089acb6ab779', validity_determination_requires_hierarchical_juridical_process, conventional).
narrative_ontology:cs_reference_frame('6a9dffa4-9d2e-4c61-92c2-089acb6ab779', tridentine_juridical_indissolubility).
narrative_ontology:cs_drift_state('6a9dffa4-9d2e-4c61-92c2-089acb6ab779', post_amoris_laetitia_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6a9dffa4-9d2e-4c61-92c2-089acb6ab779', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, diocesan_tribunal_system).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, magisterial_teaching_authority).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_professional_class).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, annulment_petitioners).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, children_of_divorced_remarried_parents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, faithful_who_remained_married).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_ontology_of_marriage).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, indissolubility_as_constitutive_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the doctrine that a validly ratified and consummated sacramental marriage cannot be dissolved by any human power, including the Church's own. Sets the theological premises that the tribunal system exists to apply. Holds no personal stake in individual cases but derives institutional authority and doctrinal coherence from the indissolubility claim being treated as settled ontology rather than pastoral aspiration.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, magisterial_teaching_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Administers the annulment process: canonical investigation into whether a marriage was ever validly formed. Collects fees, requires legal representation or canonical advocacy, and can take months to years to adjudicate a single case. Its institutional continuity and the professional livelihoods of canonists depend on marriage being treated as requiring formal juridical determination rather than pastoral discernment.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, diocesan_tribunal_system, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, diocesan_tribunal_system, beneficiary).

% Canon lawyers, tribunal judges, and defenders of the bond earn training, credentials, and income from operating the annulment apparatus. Their professional identity and career investment are structurally tied to the requirement that marital validity be adjudicated hierarchically rather than resolved pastorally.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_professional_class, beneficiary,
    organized, biographical, constrained, national).

% Having remarried civilly without an annulment, are barred from receiving Communion under the hierarchical reading because their first marriage is presumed still ontologically binding. Their only path back to full sacramental participation is petitioning the tribunal — a process many cannot afford, cannot access due to lost documentation or absent former spouses, or find retraumatizing. Leaving the Church entirely is the practical exit, at the cost of the religious identity and community they may still hold.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics, payer,
    powerless, biographical, trapped, local).

% Enter the tribunal process seeking a declaration of nullity. Bear the burden of proof, the cost of canonical representation, the emotional cost of relitigating the failure of a marriage, and multi-year delays. Success is not guaranteed even after full compliance with the process the hierarchy itself designed.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, annulment_petitioners, payer,
    powerless, biographical, constrained, national).

% Absorb the social and formational consequences of a parent's exclusion from Communion and full parish life, without having any voice in the doctrine or process that produced it.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, children_of_divorced_remarried_parents, payer,
    powerless, biographical, trapped, local).

% Experience the indissolubility teaching as coordination that stabilizes marital commitment and communal expectation; the doctrine does not extract anything from them directly and reinforces a framework they have not needed to test against the tribunal system.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, faithful_who_remained_married, beneficiary,
    moderate, biographical, mobile, local).

% Priests and lay ministers working directly with divorced and remarried Catholics who would argue for case-by-case pastoral accompaniment (the internal forum, or the discernment process associated with the civic_pastoral_reading) but whose latitude is constrained by the hierarchical reading's insistence that indissolubility is not a matter for local pastoral discretion.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_ministers_advocating_discernment, excluded,
    moderate, biographical, constrained, national).

% Study the tribunal system's caseloads, outcomes, and disparate impact by wealth and region (annulment rates and access vary enormously between dioceses and countries), producing the empirical record that either corroborates or undercuts the hierarchy's account of what the process accomplishes.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_scholars_and_sociologists_of_religion, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, doctrinally stable definition of what a valid marriage is, protecting the sacrament's meaning against ad hoc dissolution and giving the community of the faithful a shared, non-negotiable referent for marital commitment.
% TRANSFER_FUNCTION: Moves access to full sacramental participation (Communion, standing within the parish community) away from divorced and civilly remarried Catholics and toward those whose marital status the tribunal has formally validated or who never sought dissolution; moves money, time, and emotional labor from petitioners to the diocesan tribunal apparatus and its professional class.
% ABSENT_VOICES: Divorced and remarried Catholics who have left active practice rather than undergo the tribunal process are not present in synods or magisterial deliberations that reaffirm the doctrine; pastoral ministers who observe the process's disparate impact are structurally subordinate to the teaching authority that sets the terms.
% DISAPPEARANCE_RATIONALE: If hierarchical adjudication of marital validity disappeared overnight, the tribunal system and its professional class would lose their institutional function, divorced and remarried Catholics currently excluded from Communion would regain access without a juridical process, and the doctrine's role in defining eligibility for full participation in parish life would need to be replaced by some other mechanism (pastoral discernment, local bishop discretion, or no gate at all).
% FOUNDING_PROBLEM: Early and medieval Christian communities faced a real problem: without a stable, non-negotiable definition of valid marriage, powerful individuals (kings and nobles especially) could dissolve marriages at will for political or personal convenience, and the Church needed an authority structure resistant to that pressure that could not be captured by secular power.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium and canon law professional class attest the founding problem remains live — that without hierarchical, ontological indissolubility, marriage would again become subject to elite convenience and communal instability. Sociologists of religion and canon law scholars studying comparative annulment rates and the lived experience of excluded Catholics attest, from outside the tribunal system's own accounting, that the contemporary function has shifted from resisting elite capture to administering exclusion of ordinary petitioners, with substantial disparity in access by wealth, education, and diocese — a shift the hierarchy itself does not fully corroborate.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 and rising over the interval: the tribunal system's caseload, cost structure, and average case duration have grown as the institution formalized and professionalized (canon law faculties, standardized procedural norms, appellate tribunals), even as the underlying doctrinal claim has not changed. Suppression is authored at 0.71 — high, because the enforcement mechanism (denial of Eucharistic communion) is a severe and identity-central sanction with no informal workaround within the tradition; a petitioner cannot simply opt out of the tribunal system and retain full standing. Theater ratio is moderate-rising (0.42) reflecting scholarly and pastoral observation that an increasing share of tribunal activity is procedural formalization rather than substantive discernment of whether a marriage was ever validly formed — a Goodhart-style drift where processing volume and procedural compliance substitute for the pastoral function the process is nominally meant to serve.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterial and tribunal seats, this reading is coordination: a stable, incorruptible definition of marriage protecting the sacrament from ad hoc dissolution and elite capture. From the divorced-and-remarried seat, the identical structure computes as enforced extraction: a juridical gate over access to communal religious life, backed by an institution whose income and professional class depend on the gate persisting. The engine is expected to register tangled_rope from the tribunal-adjacent seats (genuine coordination function coexisting with asymmetric cost) and something closer to snare from the payer seats, which is the seat-divergence this story is built to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium and the tribunal system are structural agenda-setters and beneficiaries: they define the ontological premise and administer its consequences, with global/national institutional reach and effectively arbitrage-grade exit (they are not subject to the constraint they administer). Divorced and remarried Catholics, and annulment petitioners generally, are the structural targets: powerless, trapped or constrained exit, bearing the cost of an adjudication process whose terms they did not set and whose outcome they cannot control. The canon law professional class sits as a derivative beneficiary — organized, moderate power, whose exit is constrained by career investment in the apparatus persisting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem trio captures the genealogy tension directly: the historical founding problem (resisting secular/elite capture of marriage dissolution) is plausibly dead or substantially diminished in most contemporary jurisdictions with independent civil divorce law, yet the hierarchical adjudication apparatus persists and has, if anything, intensified its procedural formality. founding_problem_status is authored as contested rather than dead outright because the magisterium's own account (resisting relativization of marriage) remains a coherent internal claim — but it is corroborated only from within the benefiting institution. The mismatch between a status trending toward 'dead-for-the-original-purpose' and a disappearance_verdict of world_rearranges (because real people's sacramental access is currently gated by it) is exactly the capture/zombie signal this framework is designed to flag, distinct from simply calling the whole doctrine illegitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_claim_vs_institutional_administration,
    'Is the extraction measured here intrinsic to the ontological claim itself (marriage is indissoluble, full stop), or is it a contingent feature of how the tribunal system has been institutionally administered (fees, delay, procedural formalism) — such that the same ontological claim could be honored with a much lower-extraction administrative process?',
    'Comparative analysis of dioceses/eras with markedly different tribunal cost, delay, and access structures under the SAME doctrinal claim of indissolubility — if extraction varies widely while the ontological claim is held constant, extraction is administrative rather than doctrinally necessary.',
    'If extraction is separable from the ontological claim, this reading could in principle retain its distinguishing axiom while reducing measured extractiveness through administrative reform (e.g., the streamlined annulment process introduced by some recent magisterial reforms); if inseparable, high extraction is a permanent feature of holding this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_claim_vs_institutional_administration, conceptual, 'Whether measured extraction is intrinsic to the doctrine or contingent on tribunal administration.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the hierarchical_indissolubility_reading and the civic_pastoral_reading diverge — is it a disagreement about the metaphysics of marriage (ontological reality vs. relational ideal), about epistemic authority (who gets to determine validity: tribunal vs. individual conscience/local pastor), or about remedy (mandatory juridical process vs. discretionary accompaniment)? These are logically separable axes that this decomposition compresses into two readings.',
    'Systematic mapping of documented theological positions (encyclicals, synod interventions, canonical commentary) against the three axes to determine whether real-world positions cluster into exactly two readings or actually populate a richer space that a two-reading decomposition under-represents.',
    'If the metaphysics/epistemic-authority/remedy axes come apart in practice (e.g., a position holding the ontological claim but favoring discretionary pastoral remedy), the two-reading kernel decomposition may itself be a simplification requiring a third or fourth reading — this bears on whether the reading_relations declared here (coexists_with vs. forecloses) are accurately drawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the two-reading kernel decomposition captures the real structure of the doctrinal dispute or compresses a richer space.').

omega_variable(
    disparate_access_by_wealth_and_region,
    'To what extent does the tribunal system''s actual extraction burden (cost, delay, likelihood of favorable outcome) vary by petitioner wealth, education, and diocese — and does that variation undermine the hierarchy''s claim that the process is a neutral ontological inquiry rather than a resource-gated administrative filter?',
    'Comparative empirical study of annulment grant rates, average case duration, and petitioner cost across dioceses and countries, controlling for underlying case characteristics.',
    'Large, unexplained disparities by wealth/region would support the reading that current administration functions substantially as a resource-gated filter rather than a uniform ontological determination, strengthening the snare-adjacent seat computation for less-resourced petitioners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disparate_access_by_wealth_and_region, empirical, 'Whether tribunal outcomes vary by petitioner resources independent of case merits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(marr_tr_t60, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(marr_be_t60, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(marr_su_t60, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% This story and civic_pastoral_reading are sibling readings of the same marriage_sacrament kernel, not two measurements of one constraint. hierarchical_indissolubility_reading holds indissolubility as constitutive ontology requiring mandatory tribunal adjudication, producing a formal exclusion/cost structure for divorced-and-remarried Catholics (higher ε). civic_pastoral_reading holds indissolubility as an ideal subject to pastoral discernment, routing hard cases through accompaniment rather than mandatory adjudication (lower ε, different victim set). Each is authored as its own ε-invariant constraint per DP-001; they are linked here because institutional resource allocation and legitimacy pressure flow between them — magisterial reaffirmation of the hierarchical reading structurally constrains how much latitude pastoral ministers under the civic_pastoral_reading can exercise in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
