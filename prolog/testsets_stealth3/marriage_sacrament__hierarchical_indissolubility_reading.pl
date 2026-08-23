% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Sacramental Marriage Hierarchical Indissolubility Regime
 *   domain: religious/canonical/political-sociological
 *
 * SUMMARY:
 *   This story instantiates one reading of the marriage_sacrament kernel:
 *   marriage as an ontological reality — a bond constituted by consent, not
 *   merely an ideal — whose status can be pronounced on only by hierarchical
 *   adjudication operating under canon law. Under this reading,
 *   indissolubility is constitutive rather than aspirational, tribunal
 *   jurisdiction over bond-status is exclusive, and a new union contracted
 *   without a declaration of nullity places the parties outside sacramental
 *   communion. The regime carries a genuine coordination core (a fixed
 *   guarantee of bond security for the married majority, uniform status
 *   across jurisdictions, a historic protective aim against unilateral
 *   abandonment) and simultaneously imposes asymmetric, actively enforced
 *   costs on divorced members seeking new unions or release from destructive
 *   marriages. The interval maps abstractly onto the codification-to-reform
 *   era of modern canon law (roughly the 1918 code through the procedural
 *   reforms a century later); the grid is a shared abstraction, not calendar
 *   years. This file is one member of a two-story constraint family; see
 *   network.dual_formulation_note for the decomposition and the linked
 *   sibling.
 *
 * KEY AGENTS:
 *   - catholic_magisterium: Agenda-setter (institutional / identity_locked) — defines the bond doctrine and reserves all bond-status adjudication to courts under its jurisdiction
 *   - canon_tribunal_establishment: Primary beneficiary-administrator (organized / constrained) — collects fees, salaries, and institutional perpetuation from adjudication volume
 *   - faithful_married_laity: Secondary beneficiary (organized / constrained) — receives the bond-security guarantee; one divorce away from the target side of the ledger
 *   - divorced_catholics_in_new_unions: Primary target (powerless / identity_locked) — bears sacramental exclusion enforced indefinitely
 *   - divorced_catholics_denied_new_marriage: Secondary target (powerless / constrained) — bears tribunal-gate costs, delays, and uncertainty
 *   - spouses_denied_bond_dissolution: Acute target (powerless / trapped) — no release from dangerous or dead bonds short of death
 *   - internal_reform_theologians: Excluded voice (moderate / constrained) — discernment proposals circulate without adjudicative office
 *   - historians_of_canon_law: Analytical observer (analytical / analytical) — independent record of the machinery's development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.76).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.7).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Sacramental Marriage Hierarchical Indissolubility Regime").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious/canonical/political-sociological").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '18b85e45-4624-4148-9c0c-8b5da3e2af59').
narrative_ontology:cs_kernel_codification('18b85e45-4624-4148-9c0c-8b5da3e2af59', fixed_text).
narrative_ontology:cs_authority_grounding('18b85e45-4624-4148-9c0c-8b5da3e2af59', lineage).
narrative_ontology:cs_interpretation_layer_present('18b85e45-4624-4148-9c0c-8b5da3e2af59').
narrative_ontology:cs_reading_relation('18b85e45-4624-4148-9c0c-8b5da3e2af59', marriage_sacrament__civic_pastoral_reading, forecloses).
narrative_ontology:cs_axiom('18b85e45-4624-4148-9c0c-8b5da3e2af59', foundational, indissolubility_is_constitutive_not_aspirational).
narrative_ontology:cs_axiom_status(indissolubility_is_constitutive_not_aspirational, holdable).
narrative_ontology:cs_axiom_grounding('18b85e45-4624-4148-9c0c-8b5da3e2af59', indissolubility_is_constitutive_not_aspirational, theological).
narrative_ontology:cs_axiom('18b85e45-4624-4148-9c0c-8b5da3e2af59', foundational, bond_status_requires_hierarchical_adjudication).
narrative_ontology:cs_axiom_status(bond_status_requires_hierarchical_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('18b85e45-4624-4148-9c0c-8b5da3e2af59', bond_status_requires_hierarchical_adjudication, conventional).
narrative_ontology:cs_axiom('18b85e45-4624-4148-9c0c-8b5da3e2af59', secondary, new_union_after_undissolved_bond_precludes_communion).
narrative_ontology:cs_axiom_status(new_union_after_undissolved_bond_precludes_communion, holdable).
narrative_ontology:cs_axiom_grounding('18b85e45-4624-4148-9c0c-8b5da3e2af59', new_union_after_undissolved_bond_precludes_communion, theological).
narrative_ontology:cs_reference_frame('18b85e45-4624-4148-9c0c-8b5da3e2af59', constitutive_indissoluble_bond_under_apostolic_adjudication).
narrative_ontology:cs_drift_state('18b85e45-4624-4148-9c0c-8b5da3e2af59', contemporary_synodal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('18b85e45-4624-4148-9c0c-8b5da3e2af59', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, catholic_magisterium).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canon_tribunal_establishment).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, faithful_married_laity).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_in_new_unions).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_denied_new_marriage).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, spouses_denied_bond_dissolution).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, indissolubility_constitutive_doctrine).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, hierarchical_adjudication_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Popes, dicasteries, and the world's bishops teach that a consummated sacramental marriage creates a bond that nothing but death ends, and they reserve every judgment about whether a given marriage was validly formed to courts operating under canon law. Codes and disciplinary norms issue from Rome; bishops enforce them locally. What flows to this seat is adjudicative authority itself: because no married couple and no local pastor may declare a bond ended, the center of the communion becomes the indispensable interpreter of every marriage's status. Abandoning that reservation would mean repudiating the authority structure's own warrant — not available short of institutional rupture.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, catholic_magisterium, agenda_setter,
    institutional, generational, identity_locked, global).

% Diocesan tribunals employ judges, defenders of the bond, advocates, and notaries who gather evidence, interview witnesses, and issue decrees on whether marriages were validly contracted. Petitioner fees, salaries, chancery offices, and university chairs in canon law persist because adjudication volume persists. The governing rules come from above, but day-to-day case-handling norms are set inside tribunal practice. Leaving the work means leaving the canonical profession altogether.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canon_tribunal_establishment, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, canon_tribunal_establishment, agenda_setter).

% Married members in good standing receive a fixed guarantee: once their bond is declared valid, it cannot be undone by either spouse's later choice, and their standing before the community is secure without periodic recertification. They contribute financially, raise children inside the system, and serve as witnesses when neighbors' marriages come up for review. Any of them is one marital breakdown away from the paying side of the ledger, and most know it.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, faithful_married_laity, beneficiary,
    organized, biographical, constrained, global).

% Members whose first marriage ended in civil divorce and who have entered a new union without a declaration of nullity. The communion's discipline treats their new union as incompatible with full sacramental participation: they are barred from communion and from most public ministries. Staying usually means living indefinitely in sacramental exclusion while attending, contributing, and raising children in their parishes; leaving means surrendering the sacramental and communal life that structures their families' weeks and years.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_in_new_unions, payer,
    powerless, biographical, identity_locked, global).

% Divorced members not yet in new unions who wish to marry again inside the church. While the first bond stands, no priest will contract a second sacramental marriage; the only sanctioned route is petitioning a tribunal for a declaration of nullity — months to years of document gathering, witness interviews, and fees, with an uncertain outcome. Until a decree issues, the options are celibacy, a new union without sacramental recognition, or departure from the communion.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_denied_new_marriage, payer,
    powerless, biographical, constrained, regional).

% Members inside marriages that are violent, chronically destructive, or effectively dead. The framework permits separation of households and lives but never dissolution of the bond while both spouses live, so no new recognized union is possible regardless of circumstance. Safety and flourishing depend on private arrangements the system accommodates only as separation, with the door to any future recognized union closed until a spouse dies.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, spouses_denied_bond_dissolution, payer,
    powerless, biographical, trapped, global).

% Moral and pastoral theologians, many teaching in Catholic universities, who argue that individual discernment can establish when a first bond was never integrally lived, opening a path back to the sacraments without wholesale doctrinal revision. Their proposals circulate through journals, synod interventions, and advisory documents, but they hold no adjudicative office; tribunals retain exclusive jurisdiction over every case.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, internal_reform_theologians, excluded,
    moderate, biographical, constrained, regional).

% Academic scholars tracing matrimonial jurisprudence from late-antique penitential practice through the medieval decretals, the twentieth-century codifications, and recent procedural reforms. They document when adjudication centralized, how decree volumes moved, and what the machinery cost at each stage. They collect no fee and issue no decree; their publications are the main independent record of the system's development.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, historians_of_canon_law, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, canon_tribunal_establishment).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global communion on the meaning of sacramental marriage: provides a fixed, uniform norm for family formation, guarantees that no spouse can unilaterally redefine another's bond, and routes all bond-status questions through one adjudicative authority so that marital standing is identical across parishes and jurisdictions.
% TRANSFER_FUNCTION: Moves bond-status adjudication away from the married parties and their pastors to the hierarchical tribunal system; moves sacramental access away from divorced members in unrecognized new unions; confers recognized-standing status on marriages declared valid and withholds it otherwise; moves fees and professional livelihood to the tribunal apparatus.
% ABSENT_VOICES: Petitioners appear in tribunal proceedings only as suppliers of testimony, never as votes; internal reform theologians publish without adjudicative office; the Orthodox economia tradition and Protestant churches permitting new unions stand wholly outside the adjudication and would contest the exclusivity claim. Their objections are recorded in ecumenical dialogue and synodal submissions but carry no decisional weight.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, every tribunal would close, tens of thousands of pending nullity cases would lose their forum, communion discipline for divorced members in new unions would flip immediately, and the teaching office would lose the juridical keystone that makes bond-status determination its distinctive function. Marriage preparation, parish ministry to separated members, and canon-law education would all reorganize around whatever replaced adjudication; the surrounding civil legal systems would notice little, but the communion's internal architecture would rearrange.
% FOUNDING_PROBLEM: Early Christian communities confronted marriage cultures in which men could dismiss wives at will (Deuteronomic divorce provisions, Roman no-fault repudiation), leaving abandoned spouses — disproportionately women — without provision or standing. The church's refusal of dissolution distinguished it and shielded vulnerable spouses from arbitrary abandonment. Medieval canon law later added the problem of determining validity amid growing complexity of impediments, dynastic politics, and clandestine marriages, which is what made adjudication machinery necessary at scale.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by: academic historians of early Christian and medieval marriage practice (documenting the anti-abandonment genealogy and the later centralization of validity adjudication); Orthodox and Protestant ecumenical dialogue responses (attesting the protective concern as real while disputing adjudicative exclusivity); and testimony of affected laity gathered in diocesan synod and listening processes. Magisterial and tribunal self-attestation of the founding problem exists but is not counted here; the protective-core claim stands on external historical scholarship even as the necessity-of-this-machinery claim remains disputed by the sibling reading's adherents.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored high (0.76 at interval end) because the regime's heaviest costs — indefinite sacramental exclusion, tribunal delay and fees, denial of dissolution in dangerous marriages — are decoupled from any service the excluded party can decline, and the adjudication demand is largely created by the doctrine's own rigor. Suppression (0.70) is authored as a RAW structural property, unscaled by power or scope: the enforcement machinery is canon-law monopoly on bond-status determination plus sacramental denial, supplemented by internalized self-exclusion (see omega suppression_structural_vs_internalized for the mechanism split). Theater ratio (0.32) is moderate-low: adjudication performs real epistemic work on validity, but a growing share of activity is ritual reaffirmation of permanence running alongside quietly expanding informal accommodations. Accessibility collapse (0.58) and resistance (0.62) reflect the middle position: within the frame, alternatives (couple-level dissolution, competing adjudicators, pastoral case-by-case closure) are unavailable, but exit to other traditions exists and is exercised, and resistance is real — mass informal communion reception by the excluded, reform theology, synodal pressure, and declining practice. The temporal series run on ONE shared grid (t=0,20,40,60,80,100; all three metrics authored at every point): extractiveness rises monotonically as tribunal volume and the annulment economy grew on top of the doctrinal base; suppression shows an enforcement-capacity build peaking mid-interval followed by slight procedural relaxation while the formal rule persisted (an enforcement ratchet, then partial softening — not a cycle, so no intermittent-reinforcement reading applies); theater creeps upward as formal permanence is ritually affirmed over expanding informal exception-handling. Coordination type is declared identity_coordination: the regime's dominant function is boundary maintenance — determining who counts as validly bound, against evolving criteria of impediment and nullity. No boltzmann_floor_override is authored; the type default stands.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from the structural data, and the seats should diverge sharply. From the magisterium seat the arrangement is load-bearing order: without exclusive adjudication the communion has no determinate answer to what marriage is, and the teaching office's distinctive authority thins into opinion. From the tribunal seat it is a professional vocation and a functioning court system with ordinary burdens. From the faithful-married-laity seat it sits near symmetric: security purchased with contributions and conformity. From the three payer seats the same structure computes as enforced exclusion — an indefinite bar on communion, a paid and delayed gate to recognized new unions, or no exit at all from a destructive bond. The historian seat sees centuries of accretion rather than design. None of these perceptions is adjudicated by the authored claim; the divergence is the datum.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive derivation without overrides. The magisterium sits nearest the full-beneficiary end: it collects adjudicative authority itself and bears essentially none of the exclusion costs. The tribunal establishment derives low-to-moderate d — it collects fees and careers while administering, bearing little personal extraction. Faithful married laity derive moderately low d: real coordination benefit, indirect costs (contributions, witness burdens), and forward-looking exposure since any member is one marital breakdown from the paying side. The three payer groups derive high d: divorced members in new unions sit near the full-target end, amplified by identity_lock (their sacramental and communal life is fused with the community that excludes them); divorced members awaiting or deterred from adjudication are amplified by constrained exit; spouses denied dissolution are amplified by trapped status — no exit from the bond exists within the framework at all. Larger spatial scope (global communion) modestly amplifies effective extraction for targets via verification difficulty, per the engine's scope modifier. No directionality_overrides are authored: the derivation from declarations plus exit options reproduces these relationships, and overriding would substitute assertion for structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards both mislabelings. Calling the regime pure extraction ignores the coordination good that holds the married majority in place — a fixed guarantee that one's own bond cannot be undone by the other spouse's later choice, uniformity of marital status across jurisdictions, and the historic protective aim against arbitrary abandonment; remove those and the arrangement loses its acceptance base overnight. Calling it pure coordination ignores the asymmetric costs borne by a minority who never chose the tribunal's jurisdiction over their consciences, enforced through the withholding of communion. On mandatrophy: the founding problem retains a live core (protection of vulnerable spouses from unilateral abandonment is attested by external historians and echoed in ecumenical dialogue), but the machinery's present-day center of gravity — adjudicating validity amid impediment complexity — is a problem partly manufactured by the doctrine's own rigor, and the exclusion of the remarried addresses no failure of the original protective aim. Hence founding_problem_status is authored 'contested' rather than resolved: the parties dispute whether the protective core still requires this machinery, which is precisely the question the sibling reading answers differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel marriage_sacrament (reading: hierarchical_indissolubility_reading). What would the sibling reading civic_pastoral_reading change structurally, and where is the disagreement located?',
    'Authorship of the sibling story against the same structural axes: the sibling replaces tribunal-exclusive adjudication with case-by-case discernment, converts the constitutive bond into an aspirational ideal, and thereby removes the Eucharistic-exclusion mechanism and shrinks the tribunal victim set. The disagreement is located in a single element: constitutive vs. aspirational status of the bond''s persistence.',
    'If the sibling reading displaced this one, the measured exclusion extraction collapses (no category of persons is barred from communion by bond-status), the tribunal apparatus demotes from gatekeeper to optional advisory service, and this constraint''s classification would migrate toward low-extraction coordination; the current classification is valid only within this reading''s frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this file is one of two readings of the marriage_sacrament kernel; the sibling reading would restructure the victim set and delete the exclusion mechanism.').

omega_variable(
    annulment_discovery_vs_paid_gatekeeping,
    'Does tribunal nullity determination discover a pre-existing invalidity (so that nothing is dissolved and the burden is the price of an epistemic service), or does the annulment process in practice function as a paid, delayed gatekeeping toll on ending marriages?',
    'Comparative study of tribunal outcomes against the civil-divorce facts of the same unions, concession-rate patterns across tribunals and eras, and petitioner testimony on whether decrees track new information or ratify settled facts. Where decrees overwhelmingly confirm what petitioners already knew, the gate reading dominates.',
    'If the process is discovery, a large share of the measured burden is adjudication cost of a real epistemic service and effective extraction drops accordingly. If it is gatekeeping, the regime operates as a monetized toll on marital exit and effective extraction rises further, sharpening the snare-side of the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_discovery_vs_paid_gatekeeping, conceptual, 'Whether the annulment process is epistemic discovery or de facto dissolution-for-pay.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (canon-law monopoly on bond status, sacramental denial, tribunal gatekeeping) or internalized (self-exclusion from communion, perceived unworthiness persisting even where discipline technically permits reception), and in what proportion?',
    'Post-exit suppression trajectory: survey and pastoral-record analysis of members who leave the communion for traditions permitting new unions. If exclusion-feeling and abstention patterns persist after the structural barrier is removed, a substantial share is internalized.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the barrier with them after exit — and remedies aimed at the formal rules alone would overestimate relief. If predominantly structural, rule-level reform would release most of the measured pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split between structural and internalized components of the enforcement burden.').

omega_variable(
    payer_coalition_potential,
    'Could the payer groups — individually powerless, collectively numbering in the millions across the communion — convert diffuse grievance into organized pressure capable of moving the discipline, as synodal consultation episodes hint?',
    'Track organization density over time: durable advocacy associations of affected members, voting or consultative weight at synods, and whether tribunal-volume declines or regional discipline divergences follow organized campaigns rather than demographic drift.',
    'High coalition potential would raise the resistance trajectory and make enforcement-cost escalation the binding limit on the regime; negligible coalition potential leaves the payer seats structurally static regardless of numbers, sustaining high effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payer_coalition_potential, empirical, 'Whether diffuse payer grievance can aggregate into enforcement-relevant power.').

omega_variable(
    exit_identity_lock_durability,
    'What specifically binds divorced members to remain inside the communion that excludes them rather than exiting to traditions permitting new unions — and how durable is that bind across generations?',
    'Exit cohort studies: retention rates of divorced members versus matched non-divorced members, stated reasons for staying (sacramental life, family formation, communal identity), and intergenerational transmission of affiliation in households marked by exclusion.',
    'If the identity lock weakens generationally, exit drains the visible victim pool and measured suppression falls while the remaining trapped remnant experiences intensified isolation — the classification sharpens toward the snare side for whoever remains. If the lock holds, the payer population stays in place and the hybrid structure is stable indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_identity_lock_durability, empirical, 'Durability and mechanism of the identity fusion that keeps excluded members inside the regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hier_indiss_meas_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(hier_indiss_meas_tr_t0, observed).
narrative_ontology:measurement(hier_indiss_meas_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(hier_indiss_meas_tr_t20, observed).
narrative_ontology:measurement(hier_indiss_meas_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(hier_indiss_meas_tr_t40, observed).
narrative_ontology:measurement(hier_indiss_meas_tr_t60, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(hier_indiss_meas_tr_t60, observed).
narrative_ontology:measurement(hier_indiss_meas_tr_t80, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement_basis(hier_indiss_meas_tr_t80, observed).
narrative_ontology:measurement(hier_indiss_meas_tr_t100, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 100, 0.32).
narrative_ontology:measurement_basis(hier_indiss_meas_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(hier_indiss_meas_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(hier_indiss_meas_be_t0, observed).
narrative_ontology:measurement(hier_indiss_meas_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(hier_indiss_meas_be_t20, observed).
narrative_ontology:measurement(hier_indiss_meas_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(hier_indiss_meas_be_t40, observed).
narrative_ontology:measurement(hier_indiss_meas_be_t60, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 60, 0.69).
narrative_ontology:measurement_basis(hier_indiss_meas_be_t60, observed).
narrative_ontology:measurement(hier_indiss_meas_be_t80, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 80, 0.73).
narrative_ontology:measurement_basis(hier_indiss_meas_be_t80, observed).
narrative_ontology:measurement(hier_indiss_meas_be_t100, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 100, 0.76).
narrative_ontology:measurement_basis(hier_indiss_meas_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(hier_indiss_meas_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(hier_indiss_meas_su_t0, observed).
narrative_ontology:measurement(hier_indiss_meas_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(hier_indiss_meas_su_t20, observed).
narrative_ontology:measurement(hier_indiss_meas_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(hier_indiss_meas_su_t40, observed).
narrative_ontology:measurement(hier_indiss_meas_su_t60, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement_basis(hier_indiss_meas_su_t60, observed).
narrative_ontology:measurement(hier_indiss_meas_su_t80, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 80, 0.71).
narrative_ontology:measurement_basis(hier_indiss_meas_su_t80, observed).
narrative_ontology:measurement(hier_indiss_meas_su_t100, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement_basis(hier_indiss_meas_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Catholic marriage discipline' decomposes into two eps-invariant readings of the kernel marriage_sacrament. This story authors the hierarchical_indissolubility_reading: eps 0.76, victim set comprising divorced members in unrecognized new unions, divorced members gated behind tribunal adjudication, and spouses denied dissolution of dangerous bonds. The sibling story (civic_pastoral_reading) authors the pastoral-discernment reading: permanence as ideal, case-by-case discernment replacing tribunal exclusivity — a different victim set (chiefly the conscientious burden of ambiguity and inconsistent application rather than sacramental exclusion) and materially lower extraction, since the exclusion mechanism is replaced by discernment. The eps-invariance test forced the split: assessing the regime as validity-discovery service reads low extraction; assessing it as communion-access gating reads high — two structurally distinct constraints sharing a label, not one constraint with a measurement parameter. Each file links the other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
