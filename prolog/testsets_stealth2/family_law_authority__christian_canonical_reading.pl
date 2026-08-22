% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Sacramental Marriage under Ecclesiastical Authority (Christian Canonical Reading)
 *   domain: religious governance/comparative law/political theory
 *
 * SUMMARY:
 *   In historically Christian societies, marriage has been governed as a
 *   sacred bond whose validity is determined by ecclesiastical authority: the
 *   Catholic discipline holds a ratified, consummated union indissoluble and
 *   routes every question of validity through church tribunals, while
 *   Protestant polities administer marriage denominationally and most permit
 *   divorce under stated conditions. The arrangement coordinates household
 *   formation, legitimacy, and community membership across a dispersed
 *   population; it also concentrates gatekeeping power over the most
 *   consequential status in members' lives, and its permanence discipline
 *   lands on parties who cannot exit. This file instantiates ONE reading of
 *   the family_law_authority kernel — the christian_canonical_reading; the
 *   hindu_dharmashastra, muslim_shariat, parsi_zoroastrian, and
 *   secular_contractual readings are separate constraints with their own
 *   epsilon and are linked only through the network edges. The claim/metric
 *   gap is deliberate: the arrangement is CLAIMED here as tangled_rope on
 *   structural grounds, while the metrics describe its actual operation; the
 *   engine measures the divergence. KEY AGENTS (by structural relationship):
 *   - ecclesiastical_hierarchies_and_tribunals: Primary agenda-setter
 *   (institutional/arbitrage) — defines validity, adjudicates nullity,
 *   disciplines the divorced-and-remarried - denominational_governing_bodies:
 *   Variant agenda-setter (institutional/arbitrage) — administers marriage
 *   under Protestant polity with conditioned dissolution -
 *   devout_married_laity: Dual-position participant
 *   (moderate/identity_locked) — receives status and sacramental
 *   participation, bears permanence obligations -
 *   spouses_in_irreparable_marriages: Primary target (moderate/trapped) —
 *   bears the no-dissolution discipline - divorced_remarried_excluded_laity:
 *   Target (moderate/identity_locked) — bears sacramental exclusion -
 *   women_under_marital_discipline: Structurally asymmetric target
 *   (moderate/constrained) - children_of_sacramental_unions: Incidental
 *   beneficiary and bearer (powerless/trapped) - civil_family_law_regimes:
 *   Excluded rival jurisdiction (institutional/mobile) - canon_law_scholars:
 *   Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.55).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.5).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Sacramental Marriage under Ecclesiastical Authority (Christian Canonical Reading)").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious governance/comparative law/political theory").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '274bbc4e-0ee9-45d1-8bf4-196ef1bb3102').
narrative_ontology:cs_kernel_codification('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', fixed_text).
narrative_ontology:cs_authority_grounding('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', lineage).
narrative_ontology:cs_interpretation_layer_present('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102').
narrative_ontology:cs_reading_relation('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', foundational, marriage_is_indissoluble_divine_sacrament).
narrative_ontology:cs_axiom_status(marriage_is_indissoluble_divine_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', marriage_is_indissoluble_divine_sacrament, theological).
narrative_ontology:cs_axiom('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', foundational, ecclesiastical_jurisdiction_over_marital_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_jurisdiction_over_marital_validity, holdable).
narrative_ontology:cs_axiom_grounding('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', ecclesiastical_jurisdiction_over_marital_validity, conventional).
narrative_ontology:cs_axiom('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', secondary, denominational_polity_permits_conditioned_dissolution).
narrative_ontology:cs_axiom_status(denominational_polity_permits_conditioned_dissolution, holdable).
narrative_ontology:cs_axiom_grounding('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', denominational_polity_permits_conditioned_dissolution, conventional).
narrative_ontology:cs_reference_frame('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', sacramental_indissoluble_union_under_apostolic_authority).
narrative_ontology:cs_drift_state('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', contemporary_secular_family_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('274bbc4e-0ee9-45d1-8bf4-196ef1bb3102', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchies_and_tribunals).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, denominational_governing_bodies).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, devout_married_laity).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, children_of_sacramental_unions).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, spouses_in_irreparable_marriages).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, divorced_remarried_excluded_laity).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, women_under_marital_discipline).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, devout_married_laity).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, children_of_sacramental_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Popes, curial offices, diocesan bishops, and marriage tribunals define what makes a union valid, judge petitions for declarations of nullity, and set the terms on which divorced-and-remarried members may or may not receive communion. Tribunal processes carry fees and require advocate labor; the officeholder side of the desk decides which unions count. Leaving this seat would mean surrendering the office itself, not escaping its rules.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchies_and_tribunals, agenda_setter,
    institutional, generational, arbitrage, global).

% Synods, conferences, and national churches in the Protestant line administer marriage under their own polity: clergy officiate, boards rule on eligibility, and most communions permit divorce and remarriage under stated conditions. They vary the permanence discipline widely while keeping the inherited shape of officiant authority and validity questions. A body can revise its marriage canons by synod vote, which makes its position adjustable in a way the Roman discipline is not.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, denominational_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, denominational_governing_bodies, beneficiary).

% Married members in good standing receive public recognition of their union, sacramental participation, and a community that treats their household as settled. They owe permanence, exclusivity, and openness to children in return. Walking away means leaving the communion that constitutes their sacramental life, not just changing a filing office.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, devout_married_laity, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, devout_married_laity, payer).

% Children raised inside recognized unions get the continuity, legitimacy, and extended-kin recognition the community attaches to valid marriage. When a parental union collapses without dissolving, they live inside the unresolved household the discipline produces, and they have no seat in any proceeding that governs it.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, children_of_sacramental_unions, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, children_of_sacramental_unions, payer).

% Members whose marriages have factually ended — abandonment, abuse, irretrievable breakdown — cannot remarry within the community while the first bond stands. Relief runs through a nullity petition: an adversarial tribunal process with fees, witnesses, and an uncertain outcome decided by officials they do not choose. The alternative is living as a divorced person barred from new sacramental marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, spouses_in_irreparable_marriages, payer,
    moderate, biographical, trapped, global).

% Members who have entered second unions, usually civil ones, are barred from communion under Catholic discipline and face varied restrictions elsewhere. Their choice set is abandoning the new household, living in exclusion, or leaving the communion altogether — and leaving forfeits the sacramental order their whole religious life runs through.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, divorced_remarried_excluded_laity, payer,
    moderate, biographical, identity_locked, global).

% Across the tradition's history the duties of permanence, submission language, and economic dependency fell asymmetrically on wives; property and custody followed the husband by default. Contemporary discipline still lands unevenly: nullity petitions, stigma after dissolved first unions, and financial exposure after long marriages ended without remedy.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, women_under_marital_discipline, payer,
    moderate, biographical, constrained, global).

% State courts and civil registries run their own comprehensive marriage-and-divorce system. Inside this reading's frame their output is not a sacrament, so they appear only as an external jurisdiction to be accommodated by concordat or resisted — never as a co-author of what makes a union count.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, civil_family_law_regimes, excluded,
    institutional, generational, mobile, national).

% Academic canonists, historians, and sociologists of religion study the tribunal system, validity jurisprudence, and the disciplinary record. They publish on annulment rates and reform proposals without administering anything.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, canon_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchies_and_tribunals).
narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one centralized answer, for a dispersed community, to questions couples and clans otherwise negotiate unpredictably: who may marry whom (impediment and consanguinity rules), what makes a union real (form, consent, capacity), and what the community owes recognized households. Kinship alliance, inheritance order, child legitimacy, and sexual regulation are coordinated once at the center instead of per household.
% TRANSFER_FUNCTION: Moves authority over validity — and the fees, advocacy labor, deference, and obedience that travel with it — from marrying parties and their families to the ecclesiastical apparatus; moves status, legitimacy, and sacramental access to compliant unions; and fixes the costs of permanence on the spouses, landing hardest on the spouse with fewer exits.
% ABSENT_VOICES: Divorced-and-remarried members speak only as petitioners or subjects of discipline; women contesting the asymmetric duties have no seat in synod or tribunal governance; civil family-law authorities negotiate as outsiders at concordat tables rather than as co-authors of validity. The unanimity of the arrangement's internal voice reflects who was admitted to the room.
% DISAPPEARANCE_RATIONALE: Validity adjudication, tribunal employment, marriage-preparation machinery, and the communion discipline for the remarried would lose their object overnight; historically Christian societies would reorganize marriage formation entirely around civil registration, and devout communities would fragment over which replacement standard binds.
% FOUNDING_PROBLEM: Early and medieval Christianity confronted marriage customs it did not control: Roman and Germanic forms, clan-arranged alliances, easy dissolution by male repudiation, incestuous noble matches, and unprotected wives and children. The reforming church built a unified standard of valid, permanent, monogamous union and claimed the courts to enforce it — notably in the Gregorian reform's campaign to take marriage jurisdiction away from secular lords.
% FOUNDING_PROBLEM_CORROBORATION: Medieval canon-law historiography on the Gregorian reform corroborates the founding problem from outside the benefiting parties: the jurisdictional struggle with secular rulers and clan custom is independently documented. Contemporary sociology of religion and family-law history attest the other side — the original conditions (clan-custom chaos, repudiation at will) no longer obtain where civil family law covers the field, supporting the shifted-function reading. No source outside the hierarchy attests that the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.55 because the arrangement pairs a real coordination good (centralized validity answers, household standards, child protection) with concentrated gatekeeping: tribunal fees and uncertainty, communion exclusion, and a permanence discipline whose costs fall on the parties with the fewest alternatives. Suppression is authored at 0.50 as a raw structural property — the engine scales only extractiveness — reflecting enforcement that is now mostly intra-ecclesial (sacramental denial, community standing) after civil jurisdiction was lost, with a large internalized component carried by believers themselves; the structural and internalized shares are not separable by the scalar, and the suppression_internalization_split omega carries that ambiguity. Theater ratio 0.31: validity adjudication and pastoral preparation are functional, but a growing share of activity maintains the appearance of a jurisdiction the civil order no longer needs from it. Accessibility collapse 0.65: civil marriage, other communions, and plain exit exist, but for the devout the alternatives collapse substantially once the sacramental frame is lived. Resistance 0.55: internal reform movements, quiet mass noncompliance with the communion ban, annulment-rate pressure, and the long secular displacement of church courts. Coordination type is declared identity_coordination: the arrangement's primary function is boundary maintenance — deciding which unions count as real for the community — which is membership adjudication rather than resource allocation. All three tracked series share one time grid (t=0..30, mapping roughly 1900-2020) so no metric row borrows another's endpoints; the suppression_requirement series is authored because the story specifically traces enforcement-capacity decay (church courts losing civil force), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structure. From the tribunal bench the arrangement is a jurisdiction it stewards: validity questions must be answered by someone, and answering them requires an authority. From a spouse in a collapsed marriage the same tribunal is a tollbooth on the only exit, priced in money, years, and dignity. Devout married laity straddle the line — recipients of the good and bearers of the obligation — which is why their seat carries dual roles. Same-level divergence: two laypeople of identical standing in the same parish occupy opposite seats depending on facts invisible to any global power measure — whether their marriage held, whether they can fund a nullity petition, whether their second union is recognized. The identity-lock mechanism is relational-institutional fusion: for devout laity the sacramental self — one's standing before God and community — is constituted through communion membership, so exit is not a filing change but the loss of the frame in which their obligations and hopes are defined. Were that frame to break (mass defection, doctrinal revision), the payer seats would recompute as merely constrained rather than locked, and the suppression profile would drop sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the hierarchical and denominational seats sit near the beneficiary end — they collect fees, deference, and the authority that follows from gatekeeping, and they write the rules. Devout married laity derive low d from their beneficiary listing, but their dual payer position and identity-locked exit pull them toward symmetry: they receive status and bear permanence. The three victim groups derive high d: trapped spouses (no exit while the bond stands), excluded remarried laity (identity-locked — exit costs the entire sacral order), and women under the discipline (constrained exit, historically property- and custody-bound). Children derive low d from the beneficiary listing with a trapped modifier. Inter-institutionally, the hierarchy and the civil family-law regimes face the same field with different exits: the hierarchy arbitrages through doctrinal reinterpretation (annulment theology expanded rather than indissolubility abandoned), while civil regimes went mobile — they built a parallel system and let concordats absorb the residue. Civil regimes are excluded rather than coordinated; their rivalry is the boundary the arrangement maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure extraction (the secular-critical move) erases the coordination function that made it durable: centralized validity answered real collective-action problems in household formation, legitimacy, and child protection that per-clan negotiation handled badly. Reading it as pure coordination (the official move) erases the asymmetric ledger: the permanence discipline is not borne by those who administer it. Tangled rope holds both halves, which is what lets the genealogy be read honestly: the founding problem (clan-custom chaos, repudiation at will, unprotected dependents) is largely dead wherever civil family law covers the field, yet the machinery persists with its authority claims intact — a contested-status genealogy rather than a resolved one, so the status-times-verdict mismatch flag does not fire cleanly in either direction. The classification also keeps the Protestant variant from being flattened into the Catholic pole: conditioned dissolution changes the payer set, and the catholic_protestant_variance omega marks that as a candidate decomposition rather than settling it by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the family_law_authority kernel — how would the sibling readings (hindu_dharmashastra_reading, muslim_shariat_reading, parsi_zoroastrian_reading, secular_contractual_reading) change the structural picture?',
    'Generate each sibling as its own epsilon-invariant story and compare victim sets, beneficiary seats, and enforcement machinery across readings; the disagreement is located in the source of marital validity (ecclesiastical adjudication vs dharmic samskara vs contractual consent vs community-preserving institution) and in who holds dissolution power.',
    'Under the secular_contractual_reading the beneficiary seat moves to the state and the payer set shifts to parties facing state-defined terms; under the shariat reading dissolution rights are structured differently. Cross-reading comparison, not metric adjustment within this file, resolves it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    catholic_protestant_variance,
    'Does this reading''s extraction profile track the Catholic indissolubility pole or the Protestant conditioned-divorce pole?',
    'If measurement shows the two poles yield different epsilon values, decompose per the epsilon-invariance principle into two stories: Catholic indissolubility discipline, and Protestant denominational governance with conditioned dissolution.',
    'The Catholic sub-reading would show higher extraction and a larger trapped-payer set; the Protestant sub-reading would retain validity-adjudication gatekeeping while shedding most of the no-exit burden. The single authored epsilon currently averages across the reading''s internal variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catholic_protestant_variance, empirical, 'Internal denominational variance within the reading may mask two structurally distinct constraints.').

omega_variable(
    suppression_internalization_split,
    'Is the measured suppression structural (tribunal gatekeeping, sacramental denial, community sanction) or internalized (believers'' lived conviction that the bond is indissoluble)?',
    'Post-exit trajectory: track former members who divorce and remarry outside the communion — if the sense of bindingness persists after the enforcement mechanism is out of reach, the internalized share is substantial.',
    'If internalized, effective suppression exceeds the structural measure and travels with exit; payer seats harden toward trapped regardless of formal availability of civil divorce.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural vs internalized suppression mechanism in a faith-community discipline.').

omega_variable(
    annulment_pressure_valve,
    'Does the nullity process relieve the burden the permanence discipline places on parties in collapsed marriages, or convert it into procedural rent (fees, advocacy labor, years of adversarial process) paid to the tribunal apparatus?',
    'Compare petition outcomes, direct and indirect costs, and throughput before and after the 2015 streamlining reforms; interview petitioner cohorts about what the process extracted versus returned.',
    'If the process functions as rent, gains concentrate further in the tribunal seat and the receipt surface sharpens; if it functions as relief, effective extraction for the trapped-payer seat falls below the authored scalar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_pressure_valve, empirical, 'Whether annulment machinery is a safety valve or a tollbooth.').

omega_variable(
    scope_shrinkage_per_capita_effect,
    'As civil marriage displaced canonical jurisdiction over the interval, did extraction on the remaining faithful fall with the shrinking scope, or intensify per capita as only the most committed remained inside it?',
    'Compare extraction indicators (tribunal costs, exclusion incidence, discipline cases) against membership-decline curves across the interval.',
    'Per-capita intensification would push the late-interval trajectory toward the snare boundary and date any type transition late; dilution supports the decay reading and keeps the tangled_rope classification stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_shrinkage_per_capita_effect, empirical, 'Scope-shrinkage effect on per-capita extraction for the residual faithful population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(fami_tr_t0, observed).
narrative_ontology:measurement(fami_tr_t5, family_law_authority__christian_canonical_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(fami_tr_t5, observed).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__christian_canonical_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(fami_tr_t10, observed).
narrative_ontology:measurement(fami_tr_t15, family_law_authority__christian_canonical_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(fami_tr_t15, observed).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__christian_canonical_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(fami_tr_t20, observed).
narrative_ontology:measurement(fami_tr_t25, family_law_authority__christian_canonical_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(fami_tr_t25, observed).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__christian_canonical_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(fami_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement_basis(fami_be_t0, observed).
narrative_ontology:measurement(fami_be_t5, family_law_authority__christian_canonical_reading, base_extractiveness, 5, 0.67).
narrative_ontology:measurement_basis(fami_be_t5, observed).
narrative_ontology:measurement(fami_be_t10, family_law_authority__christian_canonical_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(fami_be_t10, observed).
narrative_ontology:measurement(fami_be_t15, family_law_authority__christian_canonical_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(fami_be_t15, observed).
narrative_ontology:measurement(fami_be_t20, family_law_authority__christian_canonical_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(fami_be_t20, observed).
narrative_ontology:measurement(fami_be_t25, family_law_authority__christian_canonical_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(fami_be_t25, observed).
narrative_ontology:measurement(fami_be_t30, family_law_authority__christian_canonical_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(fami_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(fami_su_t0, observed).
narrative_ontology:measurement(fami_su_t5, family_law_authority__christian_canonical_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement_basis(fami_su_t5, observed).
narrative_ontology:measurement(fami_su_t10, family_law_authority__christian_canonical_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(fami_su_t10, observed).
narrative_ontology:measurement(fami_su_t15, family_law_authority__christian_canonical_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(fami_su_t15, observed).
narrative_ontology:measurement(fami_su_t20, family_law_authority__christian_canonical_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(fami_su_t20, observed).
narrative_ontology:measurement(fami_su_t25, family_law_authority__christian_canonical_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(fami_su_t25, observed).
narrative_ontology:measurement(fami_su_t30, family_law_authority__christian_canonical_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(fami_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% family_law_authority is a contested kernel, not a single constraint: each reading instantiates a distinct arrangement with its own epsilon, beneficiary/victim sets, and enforcement machinery. This file is the christian_canonical_reading. The sibling files author the same underlying question — who governs marital validity and dissolution — under different authority structures; epsilon differs across them because the victim sets differ (the secular reading's payers face state-defined terms, not tribunal-gated sacramental exclusion). The canonical reading historically influenced the secular reading's formation (civil systems defined themselves against or alongside canon jurisdiction), which is why the edge set links all siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
