% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto as Genuine Prophetic Revelation Ending Plural Marriage
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story instantiates the endogenous-reinterpretation reading of the
 *   1890 Manifesto kernel: the claim that Wilford Woodruff's revelation
 *   ending the official sanction of plural marriage was genuine prophetic
 *   communication, and that federal pressure was the providential occasion
 *   God used rather than the efficient cause of the change. Under this
 *   reading the Church's authority structure is not damaged by the reversal
 *   but vindicated by it — continuing revelation is shown to function exactly
 *   as claimed, adapting commanded practice to changed circumstances while
 *   preserving the deeper covenant relationship. Two sibling readings of the
 *   same kernel are NOT part of this constraint: the
 *   exogenous_override_reading treats the Manifesto as pure duress with
 *   unchanged underlying doctrine, and the hybrid_pragmatic_reading treats it
 *   as strategic ambiguity management. Each is a separate constraint with its
 *   own ε; this story's ε (0.18, low) reflects only what THIS reading's own
 *   lights see — a coordination-preserving act of legitimate authority, not
 *   an extraction event.
 *
 * KEY AGENTS:
 *   - president_woodruff_and_successors: institutional/analytical exit — issues and administers the revelation as prophet
 *   - church_hierarchy_prophetic_succession: institutional/arbitrage — the office and its future authority, primary structural beneficiary
 *   - church_membership_body: organized/constrained — accepts the reframing, retains functioning church
 *   - plural_wives_and_children_post_manifesto: powerless/trapped — bears the human cost of ambiguous post-reversal status
 *   - fundamentalist_dissenting_members: powerless/trapped — excommunicated for rejecting the reading's application
 *   - federal_government: institutional/analytical — excluded from this reading's internal causal account despite being historically present
 *   - historians_and_outside_observers: analytical/analytical — assess separability of theological and pragmatic accounts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.18).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.32).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "1890 Manifesto as Genuine Prophetic Revelation Ending Plural Marriage").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'ec9ca0f3-a7fd-407b-9ca0-7720794b8c02').
narrative_ontology:cs_kernel_codification('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', formalized).
narrative_ontology:cs_authority_grounding('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', lineage).
narrative_ontology:cs_interpretation_layer_present('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02').
narrative_ontology:cs_reading_relation('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', foundational, continuing_revelation_can_reverse_prior_commanded_practice).
narrative_ontology:cs_axiom_status(continuing_revelation_can_reverse_prior_commanded_practice, holdable).
narrative_ontology:cs_axiom_grounding('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', continuing_revelation_can_reverse_prior_commanded_practice, theological).
narrative_ontology:cs_axiom('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', foundational, federal_pressure_was_providential_occasion_not_efficient_cause).
narrative_ontology:cs_axiom_status(federal_pressure_was_providential_occasion_not_efficient_cause, holdable).
narrative_ontology:cs_axiom_grounding('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', federal_pressure_was_providential_occasion_not_efficient_cause, theological).
narrative_ontology:cs_reference_frame('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', continuing_revelation_prophetic_authority).
narrative_ontology:cs_drift_state('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', post_manifesto_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ec9ca0f3-a7fd-407b-9ca0-7720794b8c02', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy_prophetic_succession).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_membership_body).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, president_woodruff_and_successors).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, plural_wives_and_children_post_manifesto).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, fundamentalist_dissenting_members).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_legitimacy).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_as_higher_purpose_vehicle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Received and issued the Manifesto as the Church's presiding prophet, seer, and revelator; administers the reinterpretation of the church's covenant relationship to plural marriage as a divinely commanded transition rather than a doctrinal reversal. Retains and exercises the office's continuing authority to declare what God has commanded, including the commanding of the change itself.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, president_woodruff_and_successors, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, president_woodruff_and_successors, beneficiary).

% The office and apparatus of continuing prophetic authority survives intact and is in fact strengthened: the capacity to receive binding revelation that alters prior commanded practice is demonstrated and thereafter available for future doctrinal adjustments. Institutional continuity, legal standing, and statehood negotiations are preserved.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy_prophetic_succession, beneficiary,
    institutional, civilizational, arbitrage, global).

% The broader membership, most of whom were monogamous, retain a functioning church, avoid confiscation of temples and further federal prosecution, and receive a theological account (a new covenant stage) that lets them understand the change as continuous with, not a repudiation of, prior revelation. They largely accept the account without independent verification of its supernatural origin.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_membership_body, beneficiary,
    organized, generational, constrained, national).

% Existing plural families are left in an ambiguous legal and social status — some marriages quietly continued, others were abandoned, wives and children lost inheritance rights and social standing as the institution reclassified their unions as no longer sanctioned. They bear the human cost of the reversal without having authored or consented to the revelation that produced it.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, plural_wives_and_children_post_manifesto, payer,
    powerless, biographical, trapped, regional).

% Members and small splinter communities who held that the original revelation commanding plural marriage was eternal and unconditional experienced excommunication and social exile when they refused to accept the Manifesto's claimed divine origin. From within this reading's own framework they are heretics rejecting continuing revelation, but they bear the disciplinary cost of the reading's success.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, fundamentalist_dissenting_members, payer,
    powerless, generational, trapped, regional).

% Applied escalating legal pressure (Edmunds-Tucker Act, disincorporation, property seizure, disenfranchisement) that this reading treats as the occasion God used, not the cause of the revelation. The federal government's own account of the episode — that the Church capitulated to coercion — is excluded from this reading's internal narrative even though the pressure is acknowledged as historically real.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, immediate, analytical, national).

% Examine the documentary record — private diaries showing internal deliberation, the timing relative to legal escalation, subsequent continuation of some plural marriages sanctioned in secret — and assess whether the theological account of divine command is separable from the pragmatic account of institutional survival under duress.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, historians_and_outside_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy_prophetic_succession).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the Church with a theologically coherent account of how a practice once declared eternal and commanded by God could be ended without conceding that the original revelation was false or that the prophetic office is fallible in a disqualifying way — coordinating belief among members around continuing revelation as the governing doctrine.
% TRANSFER_FUNCTION: Moves legitimacy and institutional survival capacity from the discredited practice of plural marriage to the continuing-revelation doctrine itself; moves the cost of the transition onto existing plural families and dissenters who do not accept the reframing, while the hierarchy and mainstream membership retain standing, property, and social respectability.
% ABSENT_VOICES: Plural wives themselves, especially those quietly abandoned or left in legally ambiguous marriages, are not recorded as parties consulted on the revelation's content or timing; fundamentalist dissenters who affirm the same revelatory framework but reject this specific application are excommunicated rather than heard as an internal theological minority.
% DISAPPEARANCE_RATIONALE: If this reading's legitimacy claim collapsed entirely — if the Manifesto were institutionally treated as pure capitulation rather than revelation — the doctrine of continuing revelation and the prophetic office's authority to bind and loose commanded practice would be substantially destabilized, and the Church's account of its own history and authority structure would require wholesale reconstruction.
% FOUNDING_PROBLEM: The Church faced a genuine crisis of survival: federal seizure of assets, disincorporation, disenfranchisement of members, and the practical impossibility of continuing plural marriage while retaining any institutional or legal standing in the United States.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream LDS historians and church-affiliated scholars (e.g., work published through Church-sponsored historical projects) corroborate the crisis was real and largely resolved by 1896 statehood; independent secular historians outside the Church's employ (e.g., academic historians of Mormonism at non-affiliated universities) corroborate the same timeline but attribute the resolution to coercion rather than revelation — the crisis's resolution is agreed upon, its causal mechanism is not, and no source entirely outside the broader Mormon studies field independently corroborates the supernatural claim itself.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18-0.22) because, from this reading's own lights, the Manifesto is not an extraction event at all — it is the correct operation of continuing revelation, and the institution's core resource (prophetic legitimacy) is enhanced rather than depleted. Suppression is moderate and declining (0.45 to 0.32) because real disciplinary force was used against dissenters who rejected the reading (excommunication, social exclusion of fundamentalist splinters) but this suppression narrows over the interval as the reading becomes settled orthodoxy and resistance fades. Theater ratio is moderate and declining (0.30 to 0.22): some performative elements exist (public testimony-bearing about the revelation's authenticity, retrospective canonization narratives) but a genuine and substantial institutional reorganization did occur, so this is not primarily theatrical maintenance. Accessibility collapse (0.58) and resistance (0.35) are mid-range, appropriately for a rope with contested internal legitimacy rather than a mountain — alternatives (continuing plural marriage covertly, splitting into a rival church) were not fully foreclosed, as the fundamentalist schisms demonstrate, but became increasingly costly to pursue as the reading consolidated.
 *
 * DIRECTIONALITY LOGIC:
 *   The prophetic office and mainstream membership are structural beneficiaries: the office's authority to declare binding change is vindicated and strengthened, and ordinary members retain a functioning, legally secure church. Plural wives and their children, and fundamentalist dissenters, are targets: they bear the concrete costs (lost inheritance, social exile, excommunication) of a change they did not author and in some cases did not accept as legitimate, with essentially no exit that does not cost them community and family standing. Federal government is excluded from beneficiary/victim framing under this reading specifically because the reading's whole point is that federal pressure was occasion, not cause — an override entry is not used here because the structural derivation already correctly places federal government as an excluded observer rather than a beneficiary or victim within this reading's internal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists the mandatrophy trap in a specific direction: it treats the founding problem (survival under federal pressure) as dead but insists the doctrine that resolved it (continuing revelation) remains fully alive and was never a mere improvisation — so the classification here is coordination (rope) preserved through genuine reinterpretation, not a scaffold whose sunset has arrived unacknowledged and not a snare wearing revelation as camouflage for confiscated crisis. The corpus should hold this in tension with the sibling readings: if the exogenous_override_reading computes as a scaffold-that-never-sunset or a tangled_rope with the same underlying facts, that divergence across readings of one kernel is the intended structural signal, not an error to reconcile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_versus_capitulation_indistinguishability,
    'Is there any evidence external to the Church''s own testimonial record that could distinguish genuine prophetic revelation from a retrospectively theologized account of coerced institutional capitulation?',
    'Compare the private documentary record (Woodruff''s diary entries, quorum meeting minutes, correspondence in the weeks surrounding the Manifesto) against the timing and severity of federal legal escalation; assess whether the theological framing appears contemporaneously or is substantially developed in subsequent decades as the crisis recedes.',
    'If the theological framing is shown to be largely retrospective (developed well after 1890 as the crisis need for it grew), this reading''s low-extraction classification becomes harder to sustain on its own terms, since it would suggest the endogenous account is itself a later institutional product rather than a contemporaneous claim; if contemporaneous documentary evidence of genuine private conviction in revelation exists independent of legal pressure, the reading is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_versus_capitulation_indistinguishability, conceptual, 'Whether the endogenous-revelation account is separable in principle from a coerced-capitulation account given available evidence.').

omega_variable(
    beneficiary_versus_natural_authority,
    'Is the prophetic succession''s enhanced legitimacy a genuine natural consequence of exercising continuing revelation correctly, or a constructed beneficiary structure in which the institution''s own authority claims are self-certifying regardless of the revelation''s actual origin?',
    'Examine whether the doctrine of continuing revelation has ever, in comparable cases, produced outcomes costly to the institution''s own authority or resources, which would suggest the mechanism is not simply self-serving by construction.',
    'If continuing revelation only ever resolves crises in ways that preserve or enhance institutional authority, the ''beneficiary'' framing borders on false-summit dynamics (a claimed-natural process that happens to always benefit the same party); if genuine costly revelations exist in the historical record, the natural-authority framing is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_versus_natural_authority, empirical, 'Whether prophetic succession''s benefit from this episode reflects genuine function or self-certifying institutional design.').

omega_variable(
    dissenter_corroboration_asymmetry,
    'Does the exclusion of fundamentalist dissenters and abandoned plural families from the reading''s internal narrative reflect a genuine theological disagreement resolved through legitimate doctrinal authority, or a structural silencing of the only parties positioned to contest the revelation''s content on the same theological grounds the mainstream reading relies on?',
    'Review fundamentalist splinter groups'' own theological arguments (they typically affirm continuing revelation as a doctrine but dispute this specific application) to assess whether the disagreement is a genuine intra-framework dispute or reflects the reading imposing exit costs that foreclose contestation regardless of theological merit.',
    'If dissenters'' arguments are theologically coherent within the shared framework and were suppressed primarily through excommunication and social cost rather than persuasion, this reading''s suppression metric is understated relative to its true function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissenter_corroboration_asymmetry, conceptual, 'Whether dissenter exclusion reflects legitimate doctrinal resolution or foreclosed contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(marr_tr_t1892, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1892, 0.27).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1894, 0.24).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1896, 0.22).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.22).
narrative_ontology:measurement(marr_be_t1892, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1892, 0.2).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1894, 0.19).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1896, 0.17).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.45).
narrative_ontology:measurement(marr_su_t1892, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1892, 0.4).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1894, 0.36).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1896, 0.34).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.33).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.1).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the marriage_commitment_legitimacy kernel. The exogenous_override_reading treats the same historical episode as coerced capitulation with unchanged underlying doctrine (expect substantially higher extraction and suppression, framing federal government as the operative agenda-setter rather than an excluded party). The hybrid_pragmatic_reading treats it as strategic ambiguity management (expect a tangled_rope classification with moderate extraction reflecting genuine but partial coordination alongside deliberate institutional self-interest). All three share the same underlying historical facts but diverge sharply in claimed beneficiary structure, causal attribution, and computed ε — this divergence across readings of a single kernel is the intended structural output, not an inconsistency to resolve.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
