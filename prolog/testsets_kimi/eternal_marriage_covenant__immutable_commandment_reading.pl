% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132 Immutable Commandment Reading
 *   domain: religious_law_political_theology
 *
 * SUMMARY:
 *   D&C 132, a revelation received by Joseph Smith in 1843, declares plural
 *   marriage an eternal, immutable law required for exaltation in the highest
 *   degree of the celestial kingdom. This constraint story instantiates the
 *   immutable_commandment_reading of the eternal_marriage_covenant kernel:
 *   the text is treated as fixed divine legislation with no legitimate
 *   revision path, prophetic override, or accommodation to civil law. Federal
 *   anti-polygamy pressure transformed compliance into a martyrdom
 *   constraint, and the mainstream LDS church's abandonment of the practice
 *   is read by this seat as institutional apostasy. The constraint
 *   coordinates a separated fundamentalist community around salvation
 *   architecture while extracting asymmetrically from women and male
 *   practitioners through patriarchal household authority and state
 *   persecution.
 *
 * KEY AGENTS:
 *   - fundamentalist_priesthood (agenda_setter/institutional/constrained): claims unbroken lineage and administers the covenant
 *   - women_in_covenants (payer/powerless/trapped): bear the asymmetric domestic and reproductive costs of plural marriage with near-zero exit
 *   - male_covenanters (payer/moderate/identity_locked): practice polygamy under federal threat; identity fused with the commandment
 *   - federal_government (observer/institutional/analytical): enforces criminal prohibition, raising the price of compliance
 *   - mainstream_lds_leadership (excluded/institutional/analytical): has renounced the immutable reading and is not recognized by fundamentalists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.82).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.88).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132 Immutable Commandment Reading").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law_political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, '2b3aa141-efe3-4f70-a87f-b63e72ca4d10').
narrative_ontology:cs_kernel_codification('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', fixed_text).
narrative_ontology:cs_authority_grounding('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', lineage).
narrative_ontology:cs_reading_relation('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', foundational, polygamy_required_for_celestial_exaltation).
narrative_ontology:cs_axiom_status(polygamy_required_for_celestial_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', polygamy_required_for_celestial_exaltation, theological).
narrative_ontology:cs_axiom('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', foundational, section_132_not_subject_to_prophetic_override).
narrative_ontology:cs_axiom_status(section_132_not_subject_to_prophetic_override, holdable).
narrative_ontology:cs_axiom_grounding('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', section_132_not_subject_to_prophetic_override, theological).
narrative_ontology:cs_reference_frame('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', joseph_smith_revelation_frame).
narrative_ontology:cs_drift_state('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', post_manifesto_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('2b3aa141-efe3-4f70-a87f-b63e72ca4d10', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, fundamentalist_priesthood).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, women_in_covenants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, male_covenanters).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, patriarchal_priesthood_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims unbroken priesthood lineage from Joseph Smith and administers plural marriage as the sole path to exaltation. Sets marriage arrangements, determines doctrinal compliance, and enforces community boundaries through fellowship withdrawal and exclusion.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, fundamentalist_priesthood, agenda_setter,
    institutional, generational, constrained, national).

% Enter plural marriages as religious duty, with limited autonomy in partner selection or household structure. Their reproductive and domestic labor supports the household unit; exit means loss of family, children, salvation, and community with no independent economic base.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, women_in_covenants, payer,
    powerless, biographical, trapped, local).

% Practice plural marriage as a commandment necessary for celestial glory. Bear the legal risk of federal prosecution, the financial burden of multiple households, and social ostracism outside the community. Their religious identity is fused with obedience to the principle.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, male_covenanters, payer,
    moderate, biographical, identity_locked, local).

% Enforces anti-polygamy statutes, seizes communal property, and imprisons practitioners. Does not participate in theological discourse but structurally shapes the constraint's operational cost through criminal law and incarceration.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Repudiated the immutable reading through the 1890 Manifesto and subsequent doctrinal development. Holds that the practice was suspended or superseded by living prophecy; is not recognized as authoritative by the fundamentalist community and is absent from its deliberations.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, mainstream_lds_leadership, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, fundamentalist_priesthood).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a theological framework for eternal marriage and exaltation, coordinating a separated fundamentalist community around a distinct covenant practice that binds families across generations and demarcates the faithful from apostate Christianity and the world.
% TRANSFER_FUNCTION: Moves authority over marriage, sexual access, and reproductive labor from women to male priesthood holders; moves deference, tithes, and household surplus from covenanters to the fundamentalist priesthood; moves federal criminal liability onto practicing members.
% ABSENT_VOICES: Women's theological objections are structurally suppressed within patriarchal revelation channels; children born into covenants have no voice in the marriage arrangements that govern their lives; mainstream LDS leadership rejects the reading entirely and is excluded from fundamentalist deliberation.
% DISAPPEARANCE_RATIONALE: The fundamentalist communities organize their entire social, marital, and economic structure around this commandment. Without it, the boundary between the principle and the world collapses, priesthood authority loses its distinguishing claim, and the community's reason for enduring federal persecution evaporates.
% FOUNDING_PROBLEM: How to secure eternal family bonds and exaltation in the afterlife through a new and everlasting covenant that transcends death, while establishing a holy community separated from Babylon.
% FOUNDING_PROBLEM_CORROBORATION: Fundamentalist priesthood attests the problem is live. Mainstream LDS church, federal courts, and academic observers attest the problem is either solved through alternative theological frames or not empirically adjudicable; no non-believing party corroborates the theological problem as real. State plainly that no external corroboration exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the commandment transfers autonomy, labor, and legal risk from covenanters to the priesthood while promising a non-falsifiable theological good. Suppression (0.88) is driven by the dual mechanism of internalized damnation terror and external federal imprisonment. Theater ratio (0.45) reflects that the covenant is genuinely believed but also performatively maintained as a boundary marker separating the elect from apostate Christianity. Accessibility collapse (0.85) captures the theological closure: once the text is accepted as God's voice, apostasy is damnation, so alternatives do not exist within the belief framework. Resistance (0.80) registers both federal armed enforcement and the mainstream church's doctrinal repudiation. Measurements trace intensifying extraction and suppression from the 1843 revelation through the federal suppression era to the present underground fundamentalist communities.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (fundamentalist priesthood) experiences the constraint as sacred trust and lineage duty; the payer seats (women and male covenanters) experience it as coercive salvation architecture where refusal costs eternal family and community. The federal observer sees a criminal conspiracy; the excluded mainstream church sees a superseded historical practice. These divergences are structural, not perspectival errors.
 *
 * DIRECTIONALITY LOGIC:
 *   The fundamentalist priesthood is the structural beneficiary: it collects deference, tithes, reproductive surplus, and genealogical authority without bearing the legal or domestic costs. Women in covenants are the primary victim: their directionality is near full-target because the constraint extracts bodily autonomy, household labor, and sexual access while offering a salvation good they cannot independently verify. Male covenanters are secondary payers: they receive the theological benefit of exaltation but pay federal criminal liability and household economic burden; their directionality sits above symmetric because the federal persecution falls on them while the priesthood is shielded by congregational secrecy. The federal government and mainstream leadership have analytical directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint could be misread as a rope (genuine coordination of eternal families) or a snare (pure patriarchal extraction). The tangled_rope classification captures both: there is a real coordination function (theological coherence, community survival under persecution, multigenerational family continuity) but it is inseparable from asymmetric extraction (gendered labor transfer, priesthood monopoly on salvation, and martyrdom costs borne by practitioners). Without the victims declared, the engine would compute a rope or mountain from the coordination data alone; the declared victim set forces the extraction asymmetry into the classification. The founding problem (eternal exaltation) is authored as live because the fundamentalist community still organizes around it, preventing a mandatrophy misfire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the immutable reading of D&C 132 represent the original authoritative intent, or is it a later fundamentalist reconstruction that ignores the historical prophetic override mechanism?',
    'Historical-critical analysis of reception history, combined with examination of whether the text itself contains internal limits or sunset clauses.',
    'If the original kernel included prophetic override mechanisms, the immutable reading''s foreclosure of the sibling is anachronistic and the constraint''s claimed type should shift toward identity_coordination cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the immutable reading is original or reconstructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily external federal coercion, or internalized religious fear of apostasy and damnation?',
    'Compare suppression persistence across jurisdictions with different federal enforcement levels; if suppression remains high where federal pressure is absent, it is substantially internalized.',
    'If internalized, the constraint operates as a snare through cognitive capture rather than a tangled rope with external enforcement; effective extraction is higher than structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    divine_origin_vs_human_construction,
    'Is D&C 132 a genuine divine revelation or a human theological-political construct serving patriarchal authority?',
    'No empirical resolution possible; historical-textual analysis can establish human editorial hands and political context (1843 Nauvoo power dynamics) but cannot adjudicate supernatural claims.',
    'If human construction, the constraint''s coordination function is a legitimizing narrative for extraction; classification shifts toward snare. If genuine divine law, the coordination function is cosmologically real and the extraction is the price of salvation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_origin_vs_human_construction, conceptual, 'Divine origin or human construction of the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(eter_tr_t30, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(eter_tr_t60, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(eter_tr_t90, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 90, 0.52).
narrative_ontology:measurement(eter_tr_t120, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 120, 0.5).
narrative_ontology:measurement(eter_tr_t150, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 150, 0.46).
narrative_ontology:measurement(eter_tr_t180, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 180, 0.45).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(eter_be_t30, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(eter_be_t60, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 60, 0.78).
narrative_ontology:measurement(eter_be_t90, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 90, 0.82).
narrative_ontology:measurement(eter_be_t120, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 120, 0.83).
narrative_ontology:measurement(eter_be_t150, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 150, 0.82).
narrative_ontology:measurement(eter_be_t180, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 180, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(eter_su_t30, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(eter_su_t60, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(eter_su_t90, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 90, 0.9).
narrative_ontology:measurement(eter_su_t120, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 120, 0.88).
narrative_ontology:measurement(eter_su_t150, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 150, 0.85).
narrative_ontology:measurement(eter_su_t180, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 180, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the eternal_marriage_covenant kernel, which decomposes into three structurally distinct claims: immutable_commandment (this file), prophetic_override, and temporal_accommodation. Each reading produces a different epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
