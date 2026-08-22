% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Peer-Review Gate on Legitimate Knowledge Claims
 *   domain: epistemology/science and technology studies/political theory
 *
 * SUMMARY:
 *   This story instantiates the credentialed_expertise_reading of the
 *   legitimate_knowledge_boundary kernel: legitimate knowledge derives from
 *   methodologically rigorous inquiry validated by credentialed peer review.
 *   Under this reading the boundary genuinely coordinates trust at scale
 *   (verification of technical claims a lay public cannot itself check) but
 *   the same machinery asymmetrically excludes non-credentialed knowledge
 *   producers whose work may be methodologically sound or empirically
 *   superior in local domains (traditional ecological knowledge, patient
 *   lived experience, community-based monitoring). The extraction is the
 *   transfer of epistemic authority, funding, and policy standing from
 *   non-credentialed to credentialed producers independent of comparative
 *   rigor. Sibling readings (experiential_pluralism_reading,
 *   hybrid_coproduction_reading) are separate constraint files with their own
 *   ε and stakeholder sets — this file does not average over them or hedge
 *   its ε against them, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - credentialed_researchers: primary beneficiary and co-agenda-setter (institutional/arbitrage) — collects epistemic authority and material reward from the gate holding
 *   - accrediting_professional_bodies: primary agenda-setter (institutional/arbitrage) — administers the credential barrier itself
 *   - journal_publishers: beneficiary (institutional/arbitrage) — commercializes the certification pipeline
 *   - community_knowledge_holders: primary target (powerless/trapped) — bears the extraction as dismissed or expropriated claims
 *   - independent_researchers_without_credentials: secondary target (moderate/constrained) — barred by credential status regardless of method quality
 *   - affected_lay_populations_excluded_from_deliberation: excluded voice (powerless/trapped) — governed by consensus they cannot contest
 *   - policy_regulators: beneficiary/payer (institutional/constrained) — borrows legitimacy from the gate but inherits its failures
 *   - science_and_technology_studies_scholars: analytical observer — documents the coordination/extraction structure without adjudicating the kernel dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.58).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.66).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Peer-Review Gate on Legitimate Knowledge Claims").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science and technology studies/political theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e4eb2501-9cd1-4981-9e4e-c104aa911fe2').
narrative_ontology:cs_kernel_codification('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', distributed).
narrative_ontology:cs_authority_grounding('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', expertise).
narrative_ontology:cs_interpretation_layer_present('e4eb2501-9cd1-4981-9e4e-c104aa911fe2').
narrative_ontology:cs_reading_relation('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', foundational, credentialed_methodological_review_is_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(credentialed_methodological_review_is_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', credentialed_methodological_review_is_necessary_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', secondary, expert_consensus_functions_as_reliable_truth_proxy).
narrative_ontology:cs_axiom_status(expert_consensus_functions_as_reliable_truth_proxy, holdable).
narrative_ontology:cs_axiom_grounding('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', expert_consensus_functions_as_reliable_truth_proxy, empirically_contingent).
narrative_ontology:cs_reference_frame('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', post_enlightenment_professionalized_science).
narrative_ontology:cs_drift_state('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', post_replication_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e4eb2501-9cd1-4981-9e4e-c104aa911fe2', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_researchers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, accrediting_professional_bodies).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, journal_publishers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, community_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, independent_researchers_without_credentials).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, affected_lay_populations_excluded_from_deliberation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_regulators).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_regulators).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, methodological_rigor_tracks_truth).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_consensus_is_reliable_truth_proxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold degrees, institutional affiliations, and publication records that grant standing to make truth-claims recognized by journals, courts, regulators, and media. They train the next generation of gatekeepers and sit on the review panels that decide whose work counts as knowledge. Their careers, funding, and social authority depend on the boundary holding.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_researchers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_researchers, agenda_setter).

% Set the credentialing requirements, accreditation standards, and licensing exams that define who may practice as a legitimate knowledge-producer in a discipline. They administer the entry barrier and can raise or lower it; they collect membership dues, accreditation fees, and licensing authority from maintaining the boundary.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, accrediting_professional_bodies, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Operate the peer-review pipeline that converts credentialed labor into certified knowledge artifacts. They profit from subscription and processing fees on a system whose legitimacy depends entirely on the credential-plus-review gate holding; they have no exit from the arrangement because it is their entire business model.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, journal_publishers, beneficiary,
    institutional, generational, arbitrage, global).

% Hold generations of situated, empirical knowledge about their land, bodies, or social conditions (traditional ecological knowledge, patient experience, indigenous land management) but lack the credentials or institutional access to have that knowledge certified as legitimate. Their claims are routinely dismissed as anecdote until re-validated by a credentialed researcher, at which point authorship and authority transfer away from them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, community_knowledge_holders, payer,
    powerless, generational, trapped, local).

% Conduct methodologically serious inquiry outside university or institutional affiliation — citizen scientists, unaffiliated scholars, self-taught specialists. They are structurally barred from most peer-reviewed venues regardless of the rigor of their work because the gate checks credential status as a precondition, not merely method quality.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, independent_researchers_without_credentials, payer,
    moderate, biographical, constrained, national).

% Are governed by policy decisions (public health, environmental regulation, technology deployment) justified by reference to certified expert consensus, but have no standing to contest the framing of the question, only the implementation of the answer. Would object to specific consensus positions if given a formal channel, but the credentialing gate treats their objections as noise rather than evidence.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, affected_lay_populations_excluded_from_deliberation, excluded,
    powerless, biographical, trapped, national).

% Rely on certified expert consensus to legitimate binding decisions and to deflect political liability ('we followed the science'). Benefit from the gate's authority-conferring function but also bear costs when the gate certifies consensus that later proves wrong or captured, since their own legitimacy is borrowed from it.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_regulators, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_regulators, payer).

% Study the credentialing and peer-review system itself as a social institution — documenting its coordination function, its exclusionary effects, and cases where consensus tracked institutional interest rather than truth. Their analysis feeds this reading and its sibling readings without adjudicating between them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, science_and_technology_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates trust in knowledge claims across a large, disaggregated population that cannot individually verify most technical claims: credentialing filters for baseline methodological competence, and peer review provides a second-pass quality check before a claim enters the shared stock of things treated as known.
% TRANSFER_FUNCTION: Moves epistemic authority (and the material and social rewards that ride on it — funding, policy standing, media deference, legal weight) from anyone without institutional credentials to anyone with them, regardless of the comparative rigor or truth-value of the underlying knowledge claim itself.
% ABSENT_VOICES: Community knowledge holders and lay populations affected by expert-certified policy are structurally absent from the rooms where consensus is formed; they can be consulted afterward but cannot contest the boundary condition that excluded their evidence from counting as evidence in the first place.
% DISAPPEARANCE_RATIONALE: From the credentialed-expertise reading's own vantage, if the credential-plus-review gate vanished overnight, the result would be an undifferentiated flood of claims with no reliable filter — journals, funding bodies, and regulators would have to invent some other legitimacy proxy quickly, likely reconstituting a similar gate under a different name. From the excluded seats' vantage, removing the gate would simply let existing (already-valid) community and independent knowledge finally enter deliberation on equal footing — the world would rearrange toward inclusion, not collapse into noise. The disagreement is exactly the kernel dispute this reading is one side of.
% FOUNDING_PROBLEM: Pre-institutional science suffered from unfalsifiable claims, undisclosed conflicts of interest, and no mechanism to filter cranks from careful investigators; credentialing plus peer review was built to solve exactly that verification problem at scale.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and STS scholars outside the credentialing bodies themselves attest the founding verification problem was real and substantially addressed by the mid-20th century professionalization of research; the same outside scholars (plus reproducibility-crisis researchers, themselves credentialed but studying the system critically) attest that the gate now also performs boundary-maintenance and rent-protection functions unrelated to verification — citing citation-cartel behavior, replication failures surviving peer review for years, and credentialing exams that test institutional socialization more than domain competence.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 by interval end: real, substantial, but not maximal, because the coordination function (filtering unfalsifiable or poorly designed claims) is genuine and the credentialed-expertise reading's own lights hold that rigor should matter. Suppression sits higher-adjacent (0.66) because the gate's persistence depends on active exclusion mechanisms — licensing requirements, journal rejection on credential grounds independent of content, tenure-and-citation structures that make exit costly for insiders and entry near-impossible for outsiders. Theater ratio is modest (0.28) reflecting that most peer review activity is functionally real verification work, with a growing minority of citation-cartel and prestige-signaling behavior layered on top over the interval. Accessibility collapse (0.62) reflects that once inside the credentialing system, alternative epistemic routes become progressively harder to see as legitimate, though they never fully vanish (community and independent knowledge production persists, just uncertified). Resistance (0.55) reflects substantial and organized pushback from excluded knowledge producers and STS critique, but not yet resistance capable of dismantling the gate.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed researchers, accrediting bodies, and journal publishers sit near the beneficiary end of directionality: the gate subsidizes their authority, income, and social standing, and their exit options are arbitrage-grade (they can move between institutions, journals, disciplines while remaining inside the credentialed system). Community knowledge holders and affected lay populations sit near the full-target end: trapped exit, no institutional recourse, and their knowledge is extracted (re-validated and re-authored by credentialed researchers) rather than directly credited. Independent researchers occupy a constrained middle position — moderate power, real methodological capability, but structurally blocked entry that a genuine merit-based rigor test would not produce. Policy regulators are dual-positioned: they benefit from borrowed legitimacy but pay when certified consensus fails, which is why they carry both beneficiary and payer roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (filtering unfalsifiable claims and undisclosed-interest research) is genuinely contested as live vs. dead: it remains live in domains with active fraud and low replication (parts of biomedicine, some social psychology) but is functioning as pure boundary-maintenance in domains where community or independent knowledge has repeatedly proven reliable without ever clearing the credential bar (indigenous land management, patient-reported outcomes). This is why founding_problem_status is authored as 'contested' rather than 'dead': a uniform verdict would erase the real heterogeneity of cases the credentialed-expertise reading covers. The tangled_rope classification (rather than snare) reflects that a genuine coordination function survives alongside the extraction — collapsing it to pure extraction would be exactly the mislabeling the framework exists to avoid; collapsing it to pure rope would erase the documented asymmetric costs borne by excluded knowledge producers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_as_rigor_proxy_or_boundary_maintenance,
    'Does credential status function primarily as a genuine proxy for methodological rigor, or has it become primarily a boundary-maintenance and rent-protection mechanism that has drifted loose from rigor itself?',
    'Compare peer-review acceptance/rejection outcomes for methodologically matched submissions that differ only in author credential status (blinded-credential natural experiments, where journals have run them); track replication rates of credentialed vs. rigorously-conducted non-credentialed work in domains with independent ground truth.',
    'If credential status tracks rigor closely, the coordination function dominates and the tangled_rope classification''s extraction component shrinks toward rope; if credential status has substantially decoupled from rigor, the extraction component grows and the classification drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_as_rigor_proxy_or_boundary_maintenance, empirical, 'Whether credentialing still tracks methodological quality or has become a self-referential status marker.').

omega_variable(
    kernel_reading_selection_bears_the_real_dispute,
    'Is the disagreement between this reading and its siblings (experiential_pluralism_reading, hybrid_coproduction_reading) actually a disagreement about facts (what counts as reliable knowledge production) or a disagreement about values (who should hold epistemic authority in a democracy)?',
    'This is the committer-axis question itself and is not resolvable within a single reading; it would require comparative institutional analysis across all three readings plus normative political theory about epistemic democracy, which by construction sits outside any one reading''s own lights.',
    'If empirical, evidence about knowledge-production track records could in principle converge the readings; if values-based, the readings will remain permanently coexisting positions regardless of evidence, which supports the coexists_with relation declared in cs_structure rather than forecloses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_bears_the_real_dispute, conceptual, 'Whether the kernel dispute is empirically resolvable or is an irreducible values disagreement about epistemic authority.').

omega_variable(
    false_summit_natural_expertise_framing,
    'Is ''methodological rigor requires credentials'' treated by credentialed researchers as an obviously natural fact about how knowledge works, when it is actually a historically contingent, constructed institutional arrangement that happens to benefit those same researchers?',
    'Historical and cross-cultural comparison: examine knowledge systems (pre-19th-century science, non-Western scientific traditions, contemporary citizen science with high reproducibility) that achieved rigorous, verified knowledge without a credentialing gate structurally identical to the modern one.',
    'If the credential requirement is better explained by professionalization history (guild formation, labor-market protection) than by an inherent epistemic necessity, this reading''s coordination claim weakens further and the tangled_rope''s extraction weighting should be read as understated rather than overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_expertise_framing, conceptual, 'Whether the credentialed-expertise reading naturalizes a contingent professional arrangement as an epistemic necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(legi_tr_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(legi_tr_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(legi_be_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(legi_be_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(legi_su_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(legi_su_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 32, 0.63).
narrative_ontology:measurement(legi_su_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 40, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.1).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the legitimate_knowledge_boundary kernel. credentialed_expertise_reading (this file) authors ε=0.58 for the standing credential-plus-peer-review gate as this reading's own lights assess it. experiential_pluralism_reading and hybrid_coproduction_reading are separate files with independently authored ε, beneficiary/victim structures, and classifications — they are NOT averaged with this file's values. The upstream influence in this family runs from the credentialed_expertise_reading (the historically dominant, more institutionally established reading) toward the other two, since institutional resource allocation and legitimacy conditions set by the credentialed reading structurally constrain how much uptake the pluralist and hybrid readings can achieve in practice, without logically foreclosing them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
