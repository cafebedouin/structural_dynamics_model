% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: Common Article 3 Scope Determined by Tracked Customary Practice (ICRC Custodial Reading)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions sets a minimum
 *   humanitarian floor for non-international armed conflict, but the text
 *   does not say which conflicts qualify or how far the floor reaches. Under
 *   the reading instantiated here, that scope is determined continuously by
 *   evolving state practice and opinio juris tracked through customary
 *   international law — principally via the ICRC's 2005 Customary IHL Study,
 *   its continuously updated database, and the 2016 updated Commentary. The
 *   arrangement is CLAIMED here as tangled_rope: a genuine coordination
 *   achievement (adaptive scope without treaty amendment among roughly 196
 *   states) carrying identifiable asymmetric costs (concentrated interpretive
 *   authority, crystallization lag borne by detainees, non-state parties
 *   bound without a seat in the making of the rules). The metrics are
 *   authored independently of that claim, from the arrangement's observable
 *   operation. This file is one reading of the common_article_3_scope kernel;
 *   the state-centric and expansive-human-rights siblings are separate
 *   constraints linked in network.affects_constraints, and nothing from them
 *   is averaged into this story. KEY AGENTS (by structural relationship): -
 *   international_committee_of_the_red_cross: Agenda-setting custodian
 *   (institutional/identity_locked) — runs the tracking apparatus, collects
 *   interpretive authority - major_military_powers: Primary beneficiary-payer
 *   (powerful/arbitrage) — their practice constitutes the record; they are
 *   bound by what crystallizes - capacity_constrained_states: Payer
 *   (moderate/constrained) — law shaped by practice they cannot document at
 *   scale - non_state_armed_groups: Bound payer (organized/trapped) — bound
 *   by rules they had no hand in making - detainees_in_contested_niacs:
 *   End-bearer (powerless/trapped) — protection arrives at the speed of
 *   crystallization - ihl_academic_commentariat: Secondary beneficiary
 *   (moderate/mobile) — careers ride on the tracking enterprise -
 *   human_rights_treaty_bodies: Excluded rival (institutional/trapped) —
 *   presses a fixed-floor reading from outside the process -
 *   international_courts_and_tribunals: Analytical observer
 *   (institutional/analytical) — applies the method case by case
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.55).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.38).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "Common Article 3 Scope Determined by Tracked Customary Practice (ICRC Custodial Reading)").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__icrc_customary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, 'ef002da1-4790-4e5e-ac59-942456e65a46').
narrative_ontology:cs_kernel_codification('ef002da1-4790-4e5e-ac59-942456e65a46', fixed_text).
narrative_ontology:cs_authority_grounding('ef002da1-4790-4e5e-ac59-942456e65a46', practice).
narrative_ontology:cs_interpretation_layer_present('ef002da1-4790-4e5e-ac59-942456e65a46').
narrative_ontology:cs_reading_relation('ef002da1-4790-4e5e-ac59-942456e65a46', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef002da1-4790-4e5e-ac59-942456e65a46', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('ef002da1-4790-4e5e-ac59-942456e65a46', foundational, scope_tracks_evolving_state_practice).
narrative_ontology:cs_axiom_status(scope_tracks_evolving_state_practice, holdable).
narrative_ontology:cs_axiom_grounding('ef002da1-4790-4e5e-ac59-942456e65a46', scope_tracks_evolving_state_practice, conventional).
narrative_ontology:cs_axiom('ef002da1-4790-4e5e-ac59-942456e65a46', secondary, gradual_expansion_without_treaty_amendment).
narrative_ontology:cs_axiom_status(gradual_expansion_without_treaty_amendment, holdable).
narrative_ontology:cs_axiom_grounding('ef002da1-4790-4e5e-ac59-942456e65a46', gradual_expansion_without_treaty_amendment, instrumental).
narrative_ontology:cs_reference_frame('ef002da1-4790-4e5e-ac59-942456e65a46', practice_constituted_minimum_floor).
narrative_ontology:cs_drift_state('ef002da1-4790-4e5e-ac59-942456e65a46', post_customary_study_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef002da1-4790-4e5e-ac59-942456e65a46', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_committee_of_the_red_cross).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, major_military_powers).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, ihl_academic_commentariat).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, detainees_in_contested_niacs).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, capacity_constrained_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, major_military_powers).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, customary_law_source_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, general_drafting_adequacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the machinery that converts raw wartime conduct into statements of customary law: the 2005 Customary IHL Study, its continuously updated online database, and the 2016 updated Commentary on Common Article 3. Its delegates observe conflicts, its lawyers weigh practice and asserted legal belief, and its published conclusions are cited by courts, military manuals, and tribunals worldwide. Its statutory mandate from the states parties funds and obliges this custodial role, and its organizational self-understanding has been bound to the custodian function since the nineteenth century; stepping back from it would mean redefining what the organization is for.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_committee_of_the_red_cross, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, international_committee_of_the_red_cross, beneficiary).

% Field large, professionally advised militaries whose manuals, directives, and after-action reviews supply most of the documented practice the customary record consists of. What they do and say becomes the raw material of the law; what crystallizes then binds them too, including in areas such as detention, targeting, and transfers where their preferred methods are contested. They can shape outcomes by documenting selectively, invoking persistent objection, or filling the record with favorable practice, and they contest scope through legal-adviser networks rather than leaving the system.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, major_military_powers, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, major_military_powers, payer).

% Maintain small foreign ministries and little or no dedicated military legal infrastructure. Their forces act in conflicts, but their practice goes undocumented or unpublished, so the record that determines everyone's obligations is built overwhelmingly from other states' filings. When a customary conclusion lands unfavorably, objecting costs diplomatic capital they have little of, and building a counter-record takes decades of documentation capacity they do not possess.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, capacity_constrained_states, payer,
    moderate, biographical, constrained, national).

% Are addressed directly by the customary rules for non-international armed conflict and are bound by them, but have no seat in the process that determines the rules' content. Their conduct enters the record only as compliance or violation data, never as law-making belief; no mechanism exists for their practice or objections to count toward the legal record. Leaving the legal order is not an option available to them.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, non_state_armed_groups, payer,
    organized, biographical, trapped, regional).

% Sit at the sharp end of undetermined scope: people held in transnational or otherwise disputed-conflict detention whose treatment depends on answers — does the common article reach this custody, does custom fill its gaps — that the tracking process resolves slowly and incompletely. They cannot accelerate crystallization, choose a forum, or wait out the interval in safety; the distance between the record and their protection is lived as their condition of confinement.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, detainees_in_contested_niacs, payer,
    powerless, immediate, trapped, regional).

% Produces the treatises, case notes, and symposia through which customary conclusions circulate and gain authority. Careers, chairs, and citation networks are built on the tracking enterprise, and the annual cycle of study updates, expert meetings, and commentary gives the field its calendar. Individual scholars can change methods or fields, but the profession as a whole is invested in practice-tracking remaining the operative paradigm.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, ihl_academic_commentariat, beneficiary,
    moderate, biographical, mobile, global).

% Advocate that a fixed humanitarian floor applies to any organized armed violence regardless of how the conflict is classified. They issue general comments and concluding observations pressing that view, but sit outside the customary process: they neither generate the practice record nor control its weighing, and their conclusions enter the record only as material the custodians may or may not credit.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, human_rights_treaty_bodies, excluded,
    institutional, generational, trapped, global).

% Adjudicate the scope questions the process generates — the Yugoslavia tribunal's Tadic jurisdiction decision and the World Court's Nicaragua judgment took practice-and-belief as the working method, and their successors follow. They apply the method analytically in concrete cases; they neither run the tracking apparatus nor bear its costs, and their holdings feed back into the record as further practice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__icrc_customary_reading, international_committee_of_the_red_cross).
narrative_ontology:fixing_cost_class(common_article_3_scope__icrc_customary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the update problem for a near-universal treaty regime: instead of reopening negotiation whenever warfare changes form, parties share one method — observe conduct, weigh asserted legal belief, publish what crystallizes — by which the common minimum floor's reach is ascertained and kept current across the four Geneva Conventions' common article.
% TRANSFER_FUNCTION: Moves interpretive authority toward whoever documents practice and articulates legal belief at scale — presently the custodian organization and well-resourced militaries — and moves binding obligations onto all parties to a conflict, including armed groups that never consented, as conclusions crystallize; the interval before crystallization is borne as unprotected exposure by detained and civilian populations in newly emerging conflict types.
% ABSENT_VOICES: Non-state armed groups would object that they are bound by rules whose making they were never part of — their practice enters the record only as compliance data, never as law-making belief. Affected populations in current theaters have no procedural seat at all. Proponents of the rival readings participate only as outside commentators on a process whose inputs they do not control; their objections are recorded, if at all, as material the custodians may discount.
% DISAPPEARANCE_RATIONALE: If the practice-tracking method vanished overnight, the common article's reach would freeze at the 1949 text plus whatever bilateral understandings exist; every open question — detention in transnational conflict, cyber operations, autonomous targeting — would convert into treaty-amendment fights among nearly two hundred states or into unilateral assertions by whichever belligerent moves first; the custodian's study and database, the courts' working method, and the scholarly tracking economy would all lose their object within a season.
% FOUNDING_PROBLEM: The 1949 diplomats drafting the Geneva Conventions faced civil wars in which governments would never concede a state of war and would thereby escape the treaties entirely. They answered with a deliberately general common article — no thresholds enumerated, no conflict types listed — betting that a determination method short of formal amendment could give the general text specific reach. The founding problem was: how does a fixed, minimal text acquire determinate, current scope without renegotiation?
% FOUNDING_PROBLEM_CORROBORATION: The negotiating history and the Pictet-era commentaries — produced before any current beneficiary held its custodial role — attest the deliberate generality and the intended evolution. Judicial corroboration comes from outside the beneficiary set: the Yugoslavia tribunal's Tadic decision and the World Court's Nicaragua judgment both rest scope-determination on practice and belief, as do the military manuals of otherwise opposed blocs. No corroborating source attests that the problem is dead; the strongest contrary claim — that thresholds or a fixed floor would settle it — is itself one of the sibling readings, not an external attestation.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.55 because the coordination yield is real — no treaty-amendment path exists for a near-universal regime, and the method has demonstrably extended the floor's reach (Tadic, the Study, the updated Commentary) — while its costs are equally real: interpretive authority concentrates in the custodian and in militaries able to document practice at scale; protection in newly emerging conflict types arrives only at the speed of crystallization; and armed groups are bound by conclusions they had no hand in forming. Suppression is authored at 0.38 as a raw, unscaled structural property (only extractiveness is scaled by directionality and scope in the engine's computation): the method maintains itself discursively — orthodoxy in manuals, curricula, and commentary — rather than by hard coercion, and the rival readings remain live, so alternatives are contested rather than closed. Theater ratio 0.31: the tracking core is functional, but as the Study ages a growing share of activity is reassertion and republication rather than new determination. Accessibility collapse is low (0.35) because the sibling determination methods remain available and understood; resistance is moderate-high (0.55): persistent-objector invocations, methodological challenges to the Study, and state pushback on specific customary conclusions are routine. The three measurement series share one time grid (T=0..30, approximately 1995–2025, from the Tadic decision and the Study mandate to the present), so every metric is authored at every examined point; trajectories rise gently as the apparatus consolidates and as contested domains (detention, cyber, autonomy) multiply the sites where crystallization lag bites. Identity-lock note: the custodian seat's exit is identity_locked — the ICRC's organizational self-concept has been fused with the custodial function since the nineteenth century, so reform pressure aimed at the custodian meets identity defense rather than cost-benefit recalculation.
 *
 * PERSPECTIVAL GAP:
 *   From the custodian seat the arrangement presents as faithful administration of a living law — the same structure reads as coordination it stewards. From the detainee seat in a contested conflict the identical structure reads as a wait: protection contingent on a record being built elsewhere, at a pace no one inside the cell controls. From the major-power seat it reads as useful flexibility that occasionally overreaches; from the capacity-constrained state's seat, as law made by other people's paperwork. The engine computes these divergent per-seat classifications from the structural data (power, exit, role); the divergence is the finding, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (the custodian organization, major military powers, the academic commentariat) drive those seats toward the beneficiary end of d; victim declarations (detainees in contested conflicts, non-state armed groups, capacity-constrained states) drive those seats toward the target end. Exit modulation sharpens the spread: the custodian's identity_locked exit and the detainees' and armed groups' trapped exits sit the former nearer subsidy and the latter nearer full target than power alone would predict, while the commentariat's mobile exit damps its d slightly despite its beneficiary role. One override is authored: major_military_powers derive a near-beneficiary d from their primary beneficiary declaration, but they are also genuinely bound by what crystallizes — their secondary payer position and their exposure in contested domains (detention, targeting) warrant d=0.38 rather than the derived near-beneficiary value. The override is keyed to the powerful atom, which in this story only that seat occupies.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview shows the founding problem — giving a deliberately general 1949 text determinate, current reach without renegotiation — is live, corroborated by pre-custodial negotiating history and by adverse-party jurisprudence; with disappearance_verdict=world_rearranges and status=live, no dead-mandate/zombie flag fires. The classification guards both mislabelings: reading the arrangement as pure coordination (rope) would erase the identifiable payers — the detainee waiting on crystallization, the armed group bound without voice, the state whose practice never enters the record; reading it as pure extraction (snare) would erase the coordination achievement that no treaty-amendment path could replicate and the survival of rival methods. Piton is excluded because the function has not atrophied — new conflict types keep arriving faster than the record closes them — and the theater ratio, while rising, remains a minority share of activity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the common_article_3_scope kernel — the icrc_customary_reading, under which CA3''s scope is determined by evolving state practice and opinio juris tracked through customary international law. How would the structural picture change under the sibling readings?',
    'Comparative generation of the sibling stories: the state_centric_reading freezes scope at intensity and organization thresholds (eliminating the adaptive mechanism and the custodial rents together); the expansive_human_rights_reading fixes a substantive floor applicable to any organized armed violence regardless of classification (eliminating practice-dependence and with it the crystallization-lag costs). Generating both files and diffing beneficiary/victim structures resolves the delta.',
    'Under the state-centric sibling the custodian''s authority and the academic tracking economy lose their object; under the expansive sibling the lag-harm victim class disappears while a new fixed-obligation payer class appears. Per-seat classifications computed here do not transfer to either sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the CA3-scope kernel; sibling readings instantiate different constraints.').

omega_variable(
    practice_record_selectivity,
    'Does the tracking apparatus weigh state practice neutrally, or do documentation capacity and editorial selection skew the record toward the practice of well-resourced militaries?',
    'Quantitative audit of the Customary IHL Study and database citations: distribution of cited practice by state capability tier, controlling for actual incidence of relevant conduct.',
    'If the record is skewed, the method''s legitimacy premise (neutral tracking) fails, and the burden on capacity-constrained states and non-state parties is heavier than the authored metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_record_selectivity, empirical, 'Whether the customary record is representative or capability-skewed.').

omega_variable(
    opinio_juris_authenticity,
    'Are the stated legal beliefs in the record genuine convictions that conduct is legally required, or post-hoc rationalizations of interest dressed as law?',
    'Concordance analysis: compare states'' asserted beliefs with their conduct and with their objections when roles reverse; persistent-objector patterns and selective invocation reveal rationalization rates.',
    'High rationalization rates would raise the true theater_ratio above the authored value and push the arrangement toward ritual maintenance of conclusions that practice does not support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opinio_juris_authenticity, empirical, 'Authenticity of the belief element in the customary record.').

omega_variable(
    crystallization_lag_attribution,
    'Are protection gaps in contested zones (undetermined detention regimes, novel weapons) caused by the customary method''s pace, or inherent to any pluralist law-determination system?',
    'Counterfactual comparison with the sibling mechanisms: model gap-closure timelines under a fixed-threshold regime and a fixed-substantive-floor regime against the observed customary timeline.',
    'If the lag is attributable to this method, the lag-harm counts as a cost of the constraint; if inherent, the victim class shrinks and the authored extractiveness overstates the arrangement''s cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crystallization_lag_attribution, conceptual, 'Whether crystallization-lag harms belong to this constraint''s account.').

omega_variable(
    custodian_identity_fusion,
    'Is the custodian''s defense of the practice-tracking method driven by functional assessment of alternatives, or by institutional identity fusion with the custodial role itself?',
    'Observe the custodian''s response to credible reform proposals that preserve humanitarian output while relocating the tracking function: willingness to cede the function indicates functional commitment; resistance proportional to role loss indicates identity fusion.',
    'If identity-fused, the agenda-setter seat cannot be moved by evidence of cheaper alternatives, and reform pressure must target the mandate''s funders rather than the custodian.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodian_identity_fusion, empirical, 'Institutional identity lock on the custodian seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_icrc_customary_tr_t0, common_article_3_scope__icrc_customary_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(ca3_icrc_customary_tr_t0, observed).
narrative_ontology:measurement(ca3_icrc_customary_tr_t5, common_article_3_scope__icrc_customary_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(ca3_icrc_customary_tr_t5, observed).
narrative_ontology:measurement(ca3_icrc_customary_tr_t10, common_article_3_scope__icrc_customary_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(ca3_icrc_customary_tr_t10, observed).
narrative_ontology:measurement(ca3_icrc_customary_tr_t15, common_article_3_scope__icrc_customary_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(ca3_icrc_customary_tr_t15, observed).
narrative_ontology:measurement(ca3_icrc_customary_tr_t20, common_article_3_scope__icrc_customary_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(ca3_icrc_customary_tr_t20, observed).
narrative_ontology:measurement(ca3_icrc_customary_tr_t25, common_article_3_scope__icrc_customary_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement_basis(ca3_icrc_customary_tr_t25, observed).
narrative_ontology:measurement(ca3_icrc_customary_tr_t30, common_article_3_scope__icrc_customary_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(ca3_icrc_customary_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(ca3_icrc_customary_be_t0, common_article_3_scope__icrc_customary_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(ca3_icrc_customary_be_t0, observed).
narrative_ontology:measurement(ca3_icrc_customary_be_t5, common_article_3_scope__icrc_customary_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement_basis(ca3_icrc_customary_be_t5, observed).
narrative_ontology:measurement(ca3_icrc_customary_be_t10, common_article_3_scope__icrc_customary_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement_basis(ca3_icrc_customary_be_t10, observed).
narrative_ontology:measurement(ca3_icrc_customary_be_t15, common_article_3_scope__icrc_customary_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement_basis(ca3_icrc_customary_be_t15, observed).
narrative_ontology:measurement(ca3_icrc_customary_be_t20, common_article_3_scope__icrc_customary_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(ca3_icrc_customary_be_t20, observed).
narrative_ontology:measurement(ca3_icrc_customary_be_t25, common_article_3_scope__icrc_customary_reading, base_extractiveness, 25, 0.54).
narrative_ontology:measurement_basis(ca3_icrc_customary_be_t25, observed).
narrative_ontology:measurement(ca3_icrc_customary_be_t30, common_article_3_scope__icrc_customary_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(ca3_icrc_customary_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(ca3_icrc_customary_su_t0, common_article_3_scope__icrc_customary_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(ca3_icrc_customary_su_t0, observed).
narrative_ontology:measurement(ca3_icrc_customary_su_t5, common_article_3_scope__icrc_customary_reading, suppression_requirement, 5, 0.31).
narrative_ontology:measurement_basis(ca3_icrc_customary_su_t5, observed).
narrative_ontology:measurement(ca3_icrc_customary_su_t10, common_article_3_scope__icrc_customary_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(ca3_icrc_customary_su_t10, observed).
narrative_ontology:measurement(ca3_icrc_customary_su_t15, common_article_3_scope__icrc_customary_reading, suppression_requirement, 15, 0.34).
narrative_ontology:measurement_basis(ca3_icrc_customary_su_t15, observed).
narrative_ontology:measurement(ca3_icrc_customary_su_t20, common_article_3_scope__icrc_customary_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(ca3_icrc_customary_su_t20, observed).
narrative_ontology:measurement(ca3_icrc_customary_su_t25, common_article_3_scope__icrc_customary_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement_basis(ca3_icrc_customary_su_t25, observed).
narrative_ontology:measurement(ca3_icrc_customary_su_t30, common_article_3_scope__icrc_customary_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(ca3_icrc_customary_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, information_standard).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% Family member of the common_article_3_scope kernel decomposition: 'CA3 scope' as colloquially discussed conflates three structurally distinct constraints — a threshold-classification determination (state_centric_reading), a practice-evolution determination (this file), and a fixed-substantive-floor determination (expansive_human_rights_reading). Each carries its own epsilon, beneficiaries, and victims; they are linked rather than merged because measuring scope-determination-by-practice yields a stable, distinct epsilon that neither sibling shares. This reading is upstream of the expansive sibling in one respect: the practice record it produces is the evidentiary base the expansive reading draws on, hence the influences edge in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__icrc_customary_reading, powerful, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
