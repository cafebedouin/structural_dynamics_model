% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Open Source as Superior Methodology, Proprietary as Legitimate Alternative
 *   domain: software_engineering/political_economy
 *
 * SUMMARY:
 *   This story instantiates the pragmatic-openness reading of the
 *   software_control_legitimacy kernel: software control is a development
 *   methodology choice, and both open source and proprietary models are
 *   legitimate, with open source having an empirical (not moral) edge from
 *   peer review and collaboration. This is one of four readings of the same
 *   kernel — freedom_imperative_reading holds proprietary control is
 *   ethically illegitimate; property_rights_reading holds restriction is a
 *   legitimate property right and open licensing is supererogatory
 *   generosity; commons_reading treats the whole question as one of
 *   negotiated collective governance of shared infrastructure. Each reading
 *   is authored as its own constraint with its own ε, beneficiary/victim
 *   structure, and type; this file authors only the pragmatic-openness
 *   reading and does not average across siblings.
 *
 * KEY AGENTS:
 *   - open_source_maintainers: primary beneficiary (moderate/mobile) — unpaid labor exchanged for review and reputation
 *   - proprietary_vendors: co-equal beneficiary (powerful/mobile) — licensed commercial model treated as equally legitimate
 *   - software_developers: beneficiary (moderate/mobile) — free methodology choice without legitimacy penalty
 *   - software_users: beneficiary (organized/mobile) — choose on quality/support merits across both models
 *   - free_software_advocates: excluded voice (organized/constrained) — reject the framing as understating an ethical claim
 *   - software_engineering_researchers: analytical observer — study empirical quality differentials
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.18).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.12).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Open Source as Superior Methodology, Proprietary as Legitimate Alternative").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, '899acc1c-453a-49dd-9453-5a2b65c05291').
narrative_ontology:cs_kernel_codification('899acc1c-453a-49dd-9453-5a2b65c05291', distributed).
narrative_ontology:cs_authority_grounding('899acc1c-453a-49dd-9453-5a2b65c05291', distributed).
narrative_ontology:cs_reading_relation('899acc1c-453a-49dd-9453-5a2b65c05291', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('899acc1c-453a-49dd-9453-5a2b65c05291', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('899acc1c-453a-49dd-9453-5a2b65c05291', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('899acc1c-453a-49dd-9453-5a2b65c05291', foundational, development_model_is_engineering_choice_not_ethical_claim).
narrative_ontology:cs_axiom_status(development_model_is_engineering_choice_not_ethical_claim, holdable).
narrative_ontology:cs_axiom_grounding('899acc1c-453a-49dd-9453-5a2b65c05291', development_model_is_engineering_choice_not_ethical_claim, instrumental).
narrative_ontology:cs_axiom('899acc1c-453a-49dd-9453-5a2b65c05291', secondary, peer_review_produces_measurable_quality_advantage).
narrative_ontology:cs_axiom_status(peer_review_produces_measurable_quality_advantage, holdable).
narrative_ontology:cs_axiom_grounding('899acc1c-453a-49dd-9453-5a2b65c05291', peer_review_produces_measurable_quality_advantage, empirically_contingent).
narrative_ontology:cs_reference_frame('899acc1c-453a-49dd-9453-5a2b65c05291', engineering_pragmatism_post_ideological_split).
narrative_ontology:cs_drift_state('899acc1c-453a-49dd-9453-5a2b65c05291', contemporary_mixed_licensing_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('899acc1c-453a-49dd-9453-5a2b65c05291', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_maintainers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and release software under open licenses, receiving peer review, bug reports, and contributions in exchange for foregoing exclusive licensing revenue. Free to also work on or contract for proprietary projects; nothing in this reading forces a choice.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_maintainers, beneficiary,
    moderate, biographical, mobile, global).

% Sell licensed, closed software and capture commercial value from controlled distribution. This reading treats their model as an equally legitimate methodology choice, not a rights violation or an inferior compromise — they compete on the same footing as open projects for developer talent and market share.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Choose freely between contributing to open projects, working for proprietary vendors, or mixing both across a career. Under this reading neither path carries a legitimacy penalty; the choice is evaluated on engineering and career merits, not ideological ones.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_developers, beneficiary,
    moderate, biographical, mobile, global).

% Select software based on quality, support, and fit for purpose, drawing from both open and proprietary offerings. Peer-reviewed open code tends toward fewer defects in this reading's account, but proprietary options remain a rational and legitimate choice where support guarantees or commercial accountability matter more.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Hold that proprietary control is an ethical wrong denying users control of their own computing, not a neutral methodology choice. This reading treats their position as one legitimate viewpoint among several rather than the correct one, which the advocates themselves would dispute as understating a rights violation.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, free_software_advocates, excluded,
    organized, generational, constrained, global).

% Study defect rates, development velocity, and collaboration patterns across open and proprietary codebases to evaluate the empirical claim that peer review produces measurably better software, without adjudicating the separate ethical or property-rights questions.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_engineering_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__pragmatic_openness_reading, diffuse).
narrative_ontology:fixing_cost_class(software_control_legitimacy__pragmatic_openness_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, non-ideological framework in which developers, firms, and users can select a development model on engineering merits — peer review and distributed testing for open projects, controlled quality assurance and commercial support for proprietary ones — without either model needing to delegitimize the other to justify its own existence.
% TRANSFER_FUNCTION: Nothing coercive is transferred between the two models under this reading: contributors give unpaid labor to open projects in exchange for reputation, tooling, and shared benefit; proprietary vendors give licensed access in exchange for payment. Value flows toward whichever model users and developers freely choose, not toward one model at the expense of the other.
% ABSENT_VOICES: Free software advocates who hold that proprietary control is inherently illegitimate are present in the discourse but their strongest claim — that this is a rights question, not a methodology question — is treated by this reading as one input among several rather than as dispositive; they would object that the pragmatic framing launders a legitimacy question into an engineering preference.
% DISAPPEARANCE_RATIONALE: If the pragmatic-coexistence framing vanished, the discourse would collapse into whichever adjacent reading fills the vacuum: either a freedom-imperative framing that delegitimizes proprietary software outright, or a property-rights framing that treats open licensing as charity rather than a peer methodology. Institutional policy (procurement rules, licensing choices, corporate open-source strategy) currently rests on the coexistence premise and would need to re-justify itself under a different premise.
% FOUNDING_PROBLEM: Early free/open-source advocacy and proprietary software business models were in direct ideological conflict (the GNU Manifesto's ethical framing versus commercial software's property framing); the pragmatic-openness reading was built to let firms, governments, and developers adopt open-source practices for engineering reasons without importing the ethical commitments of the free software movement, enabling mixed open/proprietary strategies.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the parties that most directly benefit (open-source maintainers and proprietary vendors) by empirical software-engineering research on defect rates and collaboration patterns, and by procurement and standards bodies that adopted open-source evaluation criteria on engineering rather than ideological grounds; free software advocates dispute that the problem this reading solves is the right problem to solve.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because this reading, by construction, finds no coerced transfer: value moves toward whichever model participants freely select, and neither model requires suppressing the other to sustain itself. Suppression is low (0.12) — no party is barred from choosing proprietary or open paths under this reading's own terms. Accessibility collapse is low (0.2) and resistance moderate (0.25): alternatives (the other three readings) remain fully articulable and are actively defended by their own communities, which is exactly what keeps this a live kernel contest rather than a settled mountain.
 *
 * PERSPECTIVAL GAP:
 *   Free_software_advocates and proprietary_vendors would compute this constraint very differently if either were the authoring seat: the freedom_imperative sibling reading would find proprietary_vendors as beneficiaries of an illegitimate constraint on user computing rights (a tangled_rope or snare at their seat); the property_rights sibling would find open_source_maintainers as unpaid labor propping up a norm that undercuts commercial sustainability. This reading deliberately holds neither of those verdicts — that divergence across readings is the kernel contest itself, routed to omegas rather than resolved here.
 *
 * DIRECTIONALITY LOGIC:
 *   All four listed groups are declared beneficiaries because the reading's structural claim is coexistence-without-victims: developers and users benefit from optimized quality-of-choice, open maintainers benefit from the review/reputation exchange, and proprietary vendors benefit from having their model treated as equally legitimate rather than as an ethical failure requiring justification. No victim group is declared, consistent with the expected structural delta — this is the reading's own claim, not an empirical finding that no one anywhere is harmed by any software licensing choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ideological conflict between free-software ethics and commercial software property claims) remains live — firms and developers still routinely justify mixed open/proprietary strategies against ongoing ethical objections from free software advocates. Because the problem is live rather than dead, this reading is not a zombie mandate; it continues doing real coordination work (letting procurement and engineering decisions proceed without first resolving the ethical debate) rather than merely performing settled consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodology_vs_rights_framing_choice,
    'Is ''software control'' genuinely reducible to a methodology/quality-optimization question, or does the pragmatic framing itself launder a rights question (per freedom_imperative_reading) or a property question (per property_rights_reading) into a neutral engineering preference?',
    'No empirical resolution is available — this is a framing choice about which normative register (engineering pragmatism vs. ethics vs. property rights vs. commons governance) is the correct lens for evaluating software control. Sustained tracking of which framing dominant institutions (procurement bodies, courts, standards organizations) adopt over time would show which reading has become socially dominant, without settling which is ''correct.''',
    'If the methodology framing is itself functioning as cover for one side''s substantive victory (e.g. normalizing proprietary control by refusing to call it ethically contestable), this reading''s low ε and no-victim-set claim would be understating real extraction that the freedom_imperative_reading would surface. If the framing genuinely captures how most developers and users experience the choice, the low-ε reading is accurate for the population it describes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_vs_rights_framing_choice, conceptual, 'Whether the pragmatic/methodology framing is a neutral empirical claim or a covert resolution of the underlying rights/property contest.').

omega_variable(
    empirical_quality_claim_contestability,
    'Is the claim that open source produces measurably better software through peer review a settled empirical finding, or a contested claim whose evidence varies sharply by project type, maturity, and domain?',
    'Meta-analysis of software engineering research on defect density, security vulnerability rates, and maintenance costs across matched open and proprietary codebases, controlling for project age, team size, and domain.',
    'If the quality claim is robust, it strengthens this reading''s account of why open source is a live methodology choice rather than mere ideology. If the claim is weak or domain-contingent, the reading''s stated basis for open source''s edge is thinner than the coexistence framing assumes, though the legitimacy-of-both-models claim would not itself be undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_quality_claim_contestability, empirical, 'Whether the peer-review quality advantage claimed for open source is well-supported or contested in the software engineering literature.').

omega_variable(
    cs_framing_kernel_vs_institution,
    'Should this constraint''s cs_structure be framed around the kernel as a contested legitimacy claim adjudicated by a distributed community of developers/firms/advocates (as authored here), or around a specific institutional locus (e.g. OSI license certification, FSF''s four-freedoms doctrine, or corporate open-source program offices) that actually adjudicates which projects count as ''open'' in practice?',
    'Trace whether disputes about open-source legitimacy are actually resolved by appeal to OSI/FSF certification bodies (an institutional locus with lineage-style authority) versus genuinely distributed community consensus with no single adjudicator.',
    'If an institutional locus (OSI, FSF) is the real adjudicator, authority_grounding should shift from distributed toward lineage or practice, and interpretation_layer_present would become true; the reading''s coexistence claim might then be seen as itself gatekept by a specific institutional definition of ''open'' rather than freely negotiated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_institution, conceptual, 'Alternative framing: distributed community kernel vs. institutionally-adjudicated (OSI/FSF) kernel, and what would change under each.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 25, 0.15).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 25, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_control_legitimacy__pragmatic_openness_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, information_standard).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the software_control_legitimacy kernel. All four share the same underlying contested practice (whether/how software may be controlled) but diverge in beneficiary/victim structure and ε: freedom_imperative_reading finds proprietary_vendors as beneficiaries of an ethically illegitimate constraint (higher ε, victim set = users denied computing control); property_rights_reading finds open-source norms as eroding legitimate commercial protections (victim set = rights-holders/investors); commons_reading treats control as a negotiated governance question with contested stewardship costs. This reading (pragmatic_openness) is distinguished by declaring no victim set and the lowest ε of the four, reflecting its coexistence premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
