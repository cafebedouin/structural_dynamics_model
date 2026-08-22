% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Hierarchical Indissolubility Regime for Sacramental Marriage
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   Within the Catholic sacramental economy, the
 *   hierarchical_indissolubility_reading holds marriage as an ontological
 *   reality whose indissolubility is constitutive rather than aspirational,
 *   with validity adjudicable only by hierarchical tribunal; divorced and
 *   remarried members are excluded from the Eucharist unless a tribunal
 *   declares the first bond null. This file generates THAT reading as a
 *   clean, epsilon-invariant constraint: the referent of extractiveness is
 *   the standing hierarchical arrangement itself, and the sibling
 *   civic_pastoral_reading is a separate constraint in a separate file,
 *   linked via network.affects_constraints. The colloquial label 'Catholic
 *   marriage discipline' decomposes per the epsilon-invariance principle: the
 *   hierarchical adjudication regime (this file, high epsilon, victim set =
 *   divorced/remarried and their children) and the pastoral-discernment
 *   regime (sibling file, different epsilon and victim structure) differ in
 *   epsilon because they differ in who bears costs and how exits are
 *   governed. Claim and metrics are authored independently: the claimed type
 *   states what is structurally true of the arrangement (a genuine
 *   sacramental coordination function carrying asymmetric extraction), while
 *   the metrics describe its observed operation.
 *
 * KEY AGENTS:
 *   - - divorced_remarried_catholics: Primary target (powerless/identity_locked) — bears communion exclusion, tribunal costs, and delay
 *   - - children_of_subsequent_unions: Secondary target (powerless/trapped) — inherits the irregular-family label without having chosen it
 *   - - clerical_hierarchy: Primary beneficiary and agenda-setter (institutional/identity_locked) — collects jurisdiction, deference, and cohesion; holds the levers of change
 *   - - canon_law_tribunal_system: Enforcement administrator (institutional/identity_locked) — runs the adjudication machinery; its existence is the arrangement
 *   - - faithful_laity_in_valid_marriages: Secondary beneficiary (moderate/constrained) — collects the credibility dividend, funds the system, bears its demands
 *   - - pastoral_clergy_accommodationists: Internal cost-bearer (moderate/constrained) — absorbs career risk for quiet accommodation
 *   - - lapsed_divorced_catholics: Excluded voice (powerless/mobile) — left rather than petition; absent from the record that shapes reform
 *   - - canon_law_academics: Analytical observer (analytical/analytical) — documents the norm-versus-practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.68).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.55).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Hierarchical Indissolubility Regime for Sacramental Marriage").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '69a24395-d669-445e-9619-2f76b37476b7').
narrative_ontology:cs_kernel_codification('69a24395-d669-445e-9619-2f76b37476b7', fixed_text).
narrative_ontology:cs_authority_grounding('69a24395-d669-445e-9619-2f76b37476b7', lineage).
narrative_ontology:cs_interpretation_layer_present('69a24395-d669-445e-9619-2f76b37476b7').
narrative_ontology:cs_reading_relation('69a24395-d669-445e-9619-2f76b37476b7', marriage_sacrament__civic_pastoral_reading, forecloses).
narrative_ontology:cs_axiom('69a24395-d669-445e-9619-2f76b37476b7', foundational, marriage_bond_constitutively_indissoluble).
narrative_ontology:cs_axiom_status(marriage_bond_constitutively_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('69a24395-d669-445e-9619-2f76b37476b7', marriage_bond_constitutively_indissoluble, theological).
narrative_ontology:cs_axiom('69a24395-d669-445e-9619-2f76b37476b7', secondary, marital_validity_adjudicable_only_by_hierarchical_tribunal).
narrative_ontology:cs_axiom_status(marital_validity_adjudicable_only_by_hierarchical_tribunal, holdable).
narrative_ontology:cs_axiom_grounding('69a24395-d669-445e-9619-2f76b37476b7', marital_validity_adjudicable_only_by_hierarchical_tribunal, conventional).
narrative_ontology:cs_reference_frame('69a24395-d669-445e-9619-2f76b37476b7', ontological_indissoluble_bond_order).
narrative_ontology:cs_drift_state('69a24395-d669-445e-9619-2f76b37476b7', post_amoris_laetitia_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('69a24395-d669-445e-9619-2f76b37476b7', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, clerical_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, faithful_laity_in_valid_marriages).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, children_of_subsequent_unions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, faithful_laity_in_valid_marriages).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_clergy_accommodationists).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, constitutive_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, hierarchical_marriage_jurisdiction).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_integrity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diocesan and regional courts staffed by trained judges, defenders of the bond, and notaries. They hear petitions for declarations of nullity, take testimony from former spouses and witnesses, and issue the decisions that determine who may contract a new marriage in the church and return to communion. Their dockets, procedures, and careers exist because marriages come to them for adjudication; if the adjudication function ended, the institution would have no reason to exist.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_tribunal_system, agenda_setter,
    institutional, generational, identity_locked, global).

% Popes, curial offices, and bishops who teach the indissolubility doctrine, appoint tribunal judges, and decide whether to reaffirm or relax the discipline. Marriage jurisdiction has concentrated authority over family life in their office since the medieval period; they collect the deference and cohesion that a single enforced standard produces, and they alone hold the levers that could change the arrangement. Reputational cost reaches them when enforcement controversies surface, but they bear it collectively and rarely.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, clerical_hierarchy, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, clerical_hierarchy, agenda_setter).

% Baptized members who divorced and entered a new union without a declaration of nullity. They are barred from receiving communion for as long as the new union persists. To change their status they must petition a tribunal, assemble documents and witnesses, wait months to years, and submit to a verdict on whether their first consent was valid — a process many cannot afford financially or emotionally. Leaving the church would cut them off from the sacramental life that anchors their identity, so most remain in the excluded condition, attending Mass without communion.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics, payer,
    powerless, biographical, identity_locked, global).

% Children raised in second marriages the institution classes as irregular. They did not choose their parents' prior bonds, yet they grow up inside the label: their households lack full sacramental standing, and in some parishes their access to sacramental preparation is conditioned on their parents' status. They cannot exit the family situation the rule creates.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, children_of_subsequent_unions, payer,
    powerless, biographical, trapped, global).

% Married members whose unions stand uncontested. They receive the assurance that their own vows bind and that the community's marriages mean what they say — the credibility dividend of a single enforced standard. They also fund tribunal operations through offerings and carry the discipline's demands in their own lives, including staying in difficult marriages. Their steady support is the social base that keeps the doctrine durable inside the institution.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, faithful_laity_in_valid_marriages, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, faithful_laity_in_valid_marriages, payer).

% Divorced and remarried members who concluded the tribunal route was beyond their means or endurance and left the practice entirely. They are absent from synodal consultations, tribunal statistics, and parish rolls, so their account of why the process lost them never enters the record that shapes reform. Their departure is the quietest form the arrangement's cost takes.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, lapsed_divorced_catholics, excluded,
    powerless, biographical, mobile, global).

% Parish priests who meet the excluded families week by week. Some quietly admit them to communion, steer them away from tribunals, or apply the discernment pathways loosely. When superiors notice, they face correction, unfavorable assignments, or stalled careers. They carry the day-to-day human weight of administering a discipline many of them privately doubt, at personal professional risk.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_clergy_accommodationists, payer,
    moderate, biographical, constrained, global).

% Scholars of canon law, religious history, and the sociology of religion who study tribunal records, nullity-rate trends, and the doctrine's development. They publish critiques and archival histories documenting the gap between the written norm and practiced accommodation, but they hold no vote in synods and no office in the enforcement chain.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_academics, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, clerical_hierarchy).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global community's intimate commitments around one shared standard: vows are made credible because exit is closed, families plan on permanence, and the community shares a single criterion for who stands in full communion. Adjudication resolves disputed cases under common rules instead of private declaration.
% TRANSFER_FUNCTION: Moves adjudicative fees, documentary labor, and deference from laypeople — especially petitioners — to diocesan and curial structures; moves sacramental access as a controlled good from the institution to members whose marital status conforms; takes conformity and exclusion from the divorced and remarried in the currency of communion.
% ABSENT_VOICES: Lapsed divorced Catholics who left rather than petition tribunals would testify that the process priced them out of their own community; second spouses would describe living under an irregularity label they never chose. Neither group sits in synodal assemblies or tribunal policy bodies, so the deliberating consensus forms among those who stayed.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would reopen communion to the divorced and remarried within weeks, dissolve tribunal dockets and personnel, strip the hierarchy of the marriage jurisdiction it has held since the Gregorian reform, and pluralize the community's marriage norms — the sacramental economy would reorganize around the sibling reading's pastoral-discernment pattern.
% FOUNDING_PROBLEM: Early Christian communities needed to distinguish their marriage practice from surrounding Roman divorce culture and concubinage, and to make baptismal vows credible; the medieval Gregorian reform later built papal jurisdiction partly on marriage adjudication. The arrangement was built to solve: how does a community claiming divine warrant make lifelong covenant both credible and governable?
% FOUNDING_PROBLEM_CORROBORATION: Historians of canon law — outside the benefiting parties — corroborate the jurisdiction-building genealogy of the medieval marriage courts; sociologists of religion corroborate the credibility function of strict commitment norms. The hierarchy attests the founding problem is live; the academic historiography attests that a substantial part of the original problem (Roman divorce culture) is historical, supporting the contested status.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.68: the arrangement's costs fall continuously and personally on a defined class — divorced and remarried Catholics are barred from communion until a tribunal declares their first bond null, and the process historically added fees, documentary burdens, and multi-year waits (the 2015 procedural reform registers as the dip at t=48). Suppression 0.55 is authored as a raw structural property — it is NOT scaled by power or scope in the engine's arithmetic; only extractiveness is scaled. It reflects the operative sanction (denial of the Eucharist) plus tribunal gatekeeping, down from a mid-century peak when social stigma reinforced enforcement. Theater_ratio 0.46: reaffirmation documents perform rigor while parish practice increasingly accommodates, and the tribunal process's center of gravity has migrated from guarding bonds to managing exceptions. Accessibility_collapse 0.45: civil remarriage, other communions, and the sibling pastoral reading persist as alternatives, so alternatives are degraded rather than eliminated — but for the identity-locked believer the internal alternatives collapse. Resistance 0.55: mass non-observance, dissenting pastors, and synodal contestation are real and ongoing. Coalition note: the victim class is demographically enormous; its coalition power is blunted by identity-lock and stigma, but post-synodal advocacy shows latent capacity that could convert numbers into organized power. The measurement series run on one shared time grid — every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the tribunal and hierarchy seats the arrangement is the safeguard of sacramental integrity: without adjudication, the community's marriages would lose their meaning and the Eucharist would cease to signify anything. From the divorced/remarried seat the same structure is a gate that prices re-entry to communal life behind years of process and a retrospective verdict on their consent. The engine computes divergent per-seat types from these structural positions; the divergence — not any single seat's verdict — is the datum the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection: the hierarchy collects jurisdiction, deference, and cohesion; faithful laity collect assurance that vows bind (while paying offerings and bearing the discipline's demands — hence secondary payer). Victim declarations map to borne costs: divorced/remarried bear exclusion, process costs, and delay; children of subsequent unions inherit the irregular label. Exit modulation: identity_lock pins the core victim seat near the full-target end despite nominally available civil exit; the hierarchy's identity_lock runs the other way — the institution has become its adjudicative function, binding it to maintenance. No directionality overrides were needed: beneficiary/victim data plus exit options derive the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making lifelong covenant credible and governable — is contested rather than dead: the credibility function is arguably still served, while the jurisdiction-building function has partially migrated into self-maintenance. The status=contested x verdict=world_rearranges pairing raises no zombie flag, correctly: the arrangement still organizes real behavior. The theater_ratio series (0.22 to 0.46) tracks the migration — performative reaffirmation growing as practiced discipline softens. The tangled_rope classification prevents two mislabels: a pure-extraction reading would erase the genuine coordination (credible commitment, boundary maintenance) that reform should preserve; a pure-coordination reading would erase the asymmetric costs the victim seats bear. Reform pressure therefore targets the extraction layer — process cost, exclusion practice — rather than discarding the coordination function wholesale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates only the hierarchical_indissolubility_reading of the marriage_sacrament kernel; what classification and victim set would the sibling civic_pastoral_reading instantiate, and how much of the measured extraction is attributable to the hierarchical adjudication layer rather than the sacramental kernel itself?',
    'Author the sibling story (marriage_sacrament__civic_pastoral_reading) with its own epsilon, beneficiaries, and victims; compare computed per-seat classifications across the pair.',
    'If the sibling computes with a small or empty victim set, the extraction located here belongs to the hierarchical adjudication layer specifically, sharpening reform targets; if both compute extractive, the extraction tracks the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one-of-two readings; the sibling reading would resize or relocate the victim set.').

omega_variable(
    disagreement_location_indissolubility_status,
    'Is the operative disagreement between the two readings located in the status of indissolubility (constitutive vs. aspirational), such that resolving that single element would collapse the structural differences between the readings?',
    'Doctrinal analysis: test whether a framework admitting discernment-recognized failure can retain constitutive indissolubility in any coherent form — the post-synodal dubia controversy is the live test case.',
    'If the disagreement reduces to this single element, the readings are cleanly separable constraints and the foreclosure relation holds; if it disperses across multiple elements (jurisdiction, Eucharistic discipline, tribunal monopoly), the family needs further decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_indissolubility_status, conceptual, 'Where the kernel contest is structurally located.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression keeping divorced and remarried Catholics from exiting or from claiming sacramental access structural (communion denial, tribunal gatekeeping, process cost) or internalized (exclusion experienced as deserved penance; identity fusion rendering exit unthinkable)?',
    'Post-exit trajectory study: track those who leave for other denominations or secular life; if felt obligation and self-exclusion persist after the enforcement mechanism is removed, classify the residual as internalized.',
    'If largely internalized, effective suppression exceeds the structural measure and identity_locked exit is confirmed for the core victim seat; if structural, streamlining tribunals and communion practice would release most of the suppressed demand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in the victim seat.').

omega_variable(
    nullity_rate_validity_tracking,
    'Do tribunal declarations of nullity track genuine matrimonial invalidity, or have they expanded to manage demand — performing rigor while absorbing the divorced population the doctrine excludes?',
    'Compare nullity grant rates across jurisdictions and eras against independent indicators of marriage validity at contract time (consent defects, form violations); the late-20th-century grant-rate spike in some national churches is the natural experiment.',
    'Grant-rate expansion without underlying validity change confirms metric substitution in the tribunal process (rising theater_ratio) and attributes a larger share of measured extraction to the adjudication layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nullity_rate_validity_tracking, empirical, 'Whether the tribunal process measures truth or manufactures accommodation.').

omega_variable(
    indissolubility_naturality,
    'Is lifelong indissolubility a constitutive feature of the marriage bond as such — a structural limit the discipline merely recognizes — or a disciplinary construction serving institutional interests in jurisdiction and boundary maintenance?',
    'Comparative religious history and the viability of the sibling reading: if communities holding the aspirational reading sustain comparable commitment-credibility without the hierarchical layer, the constitutive claim loses its necessity argument.',
    'If constructed, the constraint faces false-natural-law pressure and extraction attribution rises; if the tradition''s constitutive claim survives scrutiny, the coordination framing strengthens and measured excess extraction narrows to the enforcement layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indissolubility_naturality, conceptual, 'Naturality of the indissolubility claim versus constructed-discipline reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t12, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(marr_tr_t12, observed).
narrative_ontology:measurement(marr_tr_t24, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(marr_tr_t24, observed).
narrative_ontology:measurement(marr_tr_t36, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 36, 0.41).
narrative_ontology:measurement_basis(marr_tr_t36, observed).
narrative_ontology:measurement(marr_tr_t48, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 48, 0.43).
narrative_ontology:measurement_basis(marr_tr_t48, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 60, 0.46).
narrative_ontology:measurement_basis(marr_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t12, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(marr_be_t12, observed).
narrative_ontology:measurement(marr_be_t24, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement_basis(marr_be_t24, observed).
narrative_ontology:measurement(marr_be_t36, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 36, 0.71).
narrative_ontology:measurement_basis(marr_be_t36, observed).
narrative_ontology:measurement(marr_be_t48, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 48, 0.67).
narrative_ontology:measurement_basis(marr_be_t48, observed).
narrative_ontology:measurement(marr_be_t60, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(marr_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t12, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 12, 0.77).
narrative_ontology:measurement_basis(marr_su_t12, observed).
narrative_ontology:measurement(marr_su_t24, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement_basis(marr_su_t24, observed).
narrative_ontology:measurement(marr_su_t36, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 36, 0.68).
narrative_ontology:measurement_basis(marr_su_t36, observed).
narrative_ontology:measurement(marr_su_t48, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 48, 0.6).
narrative_ontology:measurement_basis(marr_su_t48, observed).
narrative_ontology:measurement(marr_su_t60, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 60, 0.54).
narrative_ontology:measurement_basis(marr_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, attachment_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Catholic marriage discipline' decomposes into two structurally distinct constraints per the epsilon-invariance principle. This file (hierarchical_indissolubility_reading) authors the standing hierarchical adjudication regime: high epsilon (0.68), victim set = divorced/remarried Catholics and children of subsequent unions, enforcement via denial of sacraments. The sibling file (civic_pastoral_reading) authors the pastoral-discernment regime: indissolubility as ideal, compassionate case-by-case discernment, structurally different victim set and epsilon. The upstream claim (constitutive indissolubility, higher doctrinal entrenchment) is cited as warrant for the downstream enforcement layer; the sibling reading attacks the upstream premise, which is why the two files must be classified separately rather than averaged. Each file links to the other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
