% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federally-Coerced Practice Suspension (Doctrine Unchanged)
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override_reading of the
 *   marriage_commitment_legitimacy kernel: the 1890 Manifesto is read as a
 *   coerced institutional capitulation extracted by federal power (the
 *   Edmunds-Tucker Act's property confiscation, disincorporation of the
 *   Church, disenfranchisement, and mass imprisonment of practicing
 *   polygamists), not a genuine theological reversal. On this reading,
 *   doctrine concerning plural marriage as an eternal principle remains
 *   formally intact within the tradition; only the practice was suspended,
 *   and suspended under duress rather than divine command. The federal
 *   government functions as the structural beneficiary, extracting
 *   institutional compliance (disbanding a marriage practice it deemed
 *   illegitimate) at the cost of the Church's temporal survival. The
 *   membership — particularly families already living in plural marriage —
 *   bears the cost of an abandonment that could not be squared with the prior
 *   half-century of teaching that the practice was essential, producing a
 *   legitimacy crisis inside the community that this reading treats as real
 *   and load-bearing, not merely a historian's retrospective gloss.
 *
 * KEY AGENTS:
 *   - federal_government: coercive agenda-setter (institutional/arbitrage) — extracts practice suspension via property seizure, disincorporation, and imprisonment
 *   - church_hierarchy_post_manifesto: institutional beneficiary of survival, structurally captured (institutional/constrained) — retains organizational continuity by capitulating on practice while claiming doctrine is unchanged
 *   - plural_marriage_practicing_membership: primary victim (powerless/trapped) — bears the material and spiritual cost of enforced separation from existing plural families
 *   - polygamous_wives_and_children: secondary victim (powerless/trapped) — lose legal and social legitimacy, inheritance standing, and household integrity
 *   - historians_and_dissenting_fundamentalist_groups: analytical/excluded observer — attest the coercion account from outside the institution's own self-narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.81).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.74).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "1890 Manifesto as Federally-Coerced Practice Suspension (Doctrine Unchanged)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, 'f9166cff-5d2e-4489-acb9-f35f89632246').
narrative_ontology:cs_kernel_codification('f9166cff-5d2e-4489-acb9-f35f89632246', fixed_text).
narrative_ontology:cs_authority_grounding('f9166cff-5d2e-4489-acb9-f35f89632246', lineage).
narrative_ontology:cs_interpretation_layer_present('f9166cff-5d2e-4489-acb9-f35f89632246').
narrative_ontology:cs_reading_relation('f9166cff-5d2e-4489-acb9-f35f89632246', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('f9166cff-5d2e-4489-acb9-f35f89632246', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('f9166cff-5d2e-4489-acb9-f35f89632246', foundational, practice_suspended_doctrine_intact_under_duress).
narrative_ontology:cs_axiom_status(practice_suspended_doctrine_intact_under_duress, holdable).
narrative_ontology:cs_axiom_grounding('f9166cff-5d2e-4489-acb9-f35f89632246', practice_suspended_doctrine_intact_under_duress, empirically_contingent).
narrative_ontology:cs_axiom('f9166cff-5d2e-4489-acb9-f35f89632246', foundational, federal_coercion_not_divine_command_caused_reversal).
narrative_ontology:cs_axiom_status(federal_coercion_not_divine_command_caused_reversal, holdable).
narrative_ontology:cs_axiom_grounding('f9166cff-5d2e-4489-acb9-f35f89632246', federal_coercion_not_divine_command_caused_reversal, empirically_contingent).
narrative_ontology:cs_reference_frame('f9166cff-5d2e-4489-acb9-f35f89632246', plural_marriage_as_eternal_commandment).
narrative_ontology:cs_drift_state('f9166cff-5d2e-4489-acb9-f35f89632246', post_edmunds_tucker_enforcement, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f9166cff-5d2e-4489-acb9-f35f89632246', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, church_hierarchy_post_manifesto).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, plural_marriage_practicing_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, polygamous_wives_and_children).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, church_hierarchy_post_manifesto).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces the Edmunds-Tucker Act and predecessor statutes: disincorporates the Church as a legal entity, confiscates Church property above a statutory exemption, disenfranchises practicing polygamists, and imprisons plural-marriage practitioners under the Edmunds Act's cohabitation provisions. Sets the terms under which the Church may regain legal recognition and its property. Faces no comparable cost from continuing the conflict and can escalate or de-escalate enforcement at will.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Issues the 1890 Manifesto advising members to conform to federal law, halting new plural marriages, in exchange for eventual restoration of property, amnesty, and Utah's path to statehood. Retains organizational continuity and eventually regains legal and civic standing, but must publicly maintain that doctrine is unchanged while privately managing a membership that includes families already living in the suspended practice; cannot exit the arrangement without ceasing to exist as a legally recognized institution in the United States.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, church_hierarchy_post_manifesto, beneficiary,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, church_hierarchy_post_manifesto, payer).

% Had entered plural marriages under explicit institutional teaching that the practice was a commandment necessary for exaltation. Now faces a choice between continuing the practice covertly under threat of prosecution and imprisonment, or dissolving covenant family relationships to comply with the institution's public capitulation. Has no meaningful exit: leaving the faith does not undo the marriages or restore legal standing, and remaining within it means absorbing a reversal the institution itself struggles to theologically justify to them.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, plural_marriage_practicing_membership, payer,
    powerless, biographical, trapped, regional).

% Bear the practical consequences most directly: loss of legal marriage status, contested inheritance and property rights, social stigma, and in many cases family separation as households are restructured to appear monogamous for legal compliance. Have essentially no independent power to affect either the federal enforcement or the Church's institutional response; their situation is determined entirely by decisions made above them.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, polygamous_wives_and_children, payer,
    powerless, biographical, trapped, local).

% Later fundamentalist splinter groups reject the Manifesto's legitimacy entirely, continuing the practice and asserting the mainstream Church abandoned a binding commandment under coercion — corroborating this reading's coercion account from a seat outside the mainstream institution's own self-narrative. Historians examining federal legislative and legal records document the coercive mechanism independently of either the Church's or the fundamentalists' interests, but are not parties who can alter the arrangement, only observe and attest to it.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, historians_and_dissenting_fundamentalist_groups, excluded,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves an acute federal-versus-institution conflict over marriage law: the Church avoids complete legal dissolution and property loss, and the federal government achieves nominal enforcement of a uniform national marriage standard, without continued mass prosecution of the entire Utah territory population.
% TRANSFER_FUNCTION: Moves compliance and practice cessation from the Church and its plural-marriage-practicing membership to the federal government, in exchange for the Church's continued legal existence, restored property, and Utah's eventual statehood; the material and relational costs of dissolving existing plural families are absorbed almost entirely by the practicing membership and their wives and children rather than by the negotiating institutions.
% ABSENT_VOICES: Practicing plural wives and their children had essentially no voice in either the federal legislative process or the Church's negotiated response; fundamentalist dissenters who rejected the Manifesto's legitimacy were subsequently excommunicated and pushed entirely outside the institution's official narrative, removing the clearest internal source of the coercion account from the Church's own historical record.
% DISAPPEARANCE_RATIONALE: Had the federal enforcement apparatus not existed or been withdrawn, the Church would very plausibly have continued the practice; Utah statehood, the eventual restoration of Church property, and the entire subsequent institutional trajectory (including the 1904 Second Manifesto and modern excommunication policy toward polygamy) are downstream of this specific coerced settlement, not of an independent doctrinal reconsideration.
% FOUNDING_PROBLEM: The federal government sought to eliminate plural marriage as a matter of national marriage-law uniformity and public morality politics; escalating legal pressure (Morrill Act 1862, Edmunds Act 1882, Edmunds-Tucker Act 1887) aimed at destroying the Church's legal and economic viability unless the practice ended.
% FOUNDING_PROBLEM_CORROBORATION: Federal legal records and legislative history attest the coercive mechanism (property seizure, disincorporation, imprisonment) independently of the Church's own account. Fundamentalist splinter groups that reject the Manifesto's legitimacy corroborate the coercion reading from outside the beneficiary institution, asserting the founding problem (federal hostility to plural marriage) was resolved by capitulation rather than by genuine doctrinal change, and that the practice-suspension persists institutionally today as settled policy rather than as an active response to an ongoing federal threat.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at interval end) because, under this reading, the federal government is not solving a coordination problem for the Church — it is extracting compliance through asymmetric legal force (Edmunds-Tucker's disincorporation and property confiscation, the 1887 escheatment of Church assets, and the credible threat of continued mass prosecution) that the Church could not resist without ceasing to exist as a legal entity. Suppression peaks sharply around the years of the property seizures and prosecutions (time_point 8, value 0.88) reflecting the acute enforcement crisis, then declines somewhat as the arrangement stabilizes into the post-Manifesto settlement — but never returns to pre-crisis levels, since ongoing federal recognition of the Church's legal status remained conditioned on continued compliance. Theater ratio rises over the same window (0.25 to a peak of 0.70) as the 'doctrine unchanged, only practice suspended' framing becomes an increasingly load-bearing performance the institution must maintain publicly while privately managing continued underground practice by some members through the 1900s — the Second Manifesto of 1904 (outside this interval but foreshadowed) is the historical marker of theater becoming policy enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's seat, the arrangement looks like law enforcement resolving a genuine coordination failure (a religious institution defying settled federal marriage law) — a rope, from its lights. From the practicing membership's seat, the identical structure is a snare: coerced abandonment of a covenant relationship under threat of family destruction and imprisonment, with no genuine coordination benefit to them. The Church hierarchy's seat sits between: it experiences institutional survival as a real benefit but experiences the manner of achieving it (public capitulation with private doctrinal preservation) as an enforced extraction it did not choose. This divergence is exactly what the engine's per-seat computation is meant to surface — the claimed_type here (tangled_rope) reflects that BOTH a real coordination story (institutional survival, cessation of a genuinely illegal-under-federal-law practice) and a real asymmetric extraction (coerced abandonment of covenant families under active legal enforcement) are simultaneously present.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government sits at the full-beneficiary/enforcer end: it extracts institutional compliance (d near the target-collecting end from its own perspective — it receives what it sought) with essentially unconstrained exit (it can walk away from the conflict at any point; the Church cannot). The Church hierarchy is directionally mixed: it benefits from organizational survival (avoiding complete legal dissolution) but pays the cost of doctrinal contortion and loss of moral authority among its most committed practicing members — hence its dual role (beneficiary + payer) with identity-locked exit options, since the institution's continued existence as itself depends on accepting the arrangement. Practicing plural-marriage families are the clearest victims: trapped exit options, powerless power atom, and the full weight of the extraction lands on them as they are asked to dissolve marriages the institution had taught them were eternally binding.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists collapsing into a pure snare or a pure rope by insisting the coordination function (federal marriage law being upheld, institutional survival being preserved) is genuinely present alongside the extraction (coerced practice suspension imposed via property confiscation and imprisonment) rather than treating one as mere cover for the other. Framing this as tangled_rope rather than snare acknowledges the Church's own institutional beneficiary status post-Manifesto (survival, eventual statehood, reintegration into American civic life) while still requiring active enforcement (federal legal apparatus) and naming a concrete victim class (practicing plural families) whose costs were not incidental but structural to the resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_kernel_reading_ambiguity,
    'Was the 1890 Manifesto a genuine revelatory reversal (endogenous_reinterpretation_reading), a coerced capitulation with doctrine unchanged (this reading), or a strategic hybrid using scope ambiguity to manage both prophetic authority and federal pressure (hybrid_pragmatic_reading)?',
    'Church-internal doctrinal record (whether plural marriage was ever formally repudiated as a theological principle vs. merely administratively suspended), contemporaneous private correspondence of Church leadership, and comparison of the Manifesto''s actual text (which addresses ''the law of the land'' rather than revoking doctrine) against later Second Manifesto (1904) enforcement patterns.',
    'If genuinely revelatory, this constraint''s claimed_type and extraction profile collapse toward rope/mountain (a natural theological correction, no exogenous victim). If exogenous override (this reading), extraction is high and directed at a coerced institution and a targeted membership. If hybrid, the extraction is present but distributed differently, with the Church itself as a co-beneficiary of the ambiguity rather than a pure victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_kernel_reading_ambiguity, conceptual, 'Which of the three kernel readings correctly characterizes the Manifesto''s structural status; this story instantiates only the exogenous_override_reading.').

omega_variable(
    doctrine_practice_separability,
    'Can ''doctrine'' and ''practice'' be cleanly separated for a commandment the Church itself once described as essential to exaltation, such that suspending practice under duress leaves doctrine genuinely ''unchanged''?',
    'Analysis of whether the Church''s official theological statements ever asserted plural marriage was contingent/administrative versus eternally binding; tracking of doctrinal language across 1852 (public announcement), 1890 (Manifesto), and 1904 (Second Manifesto) statements.',
    'If doctrine and practice are separable as this reading claims, the extraction is purely a matter of the federal government extracting institutional compliance while theological truth-claims survive intact underneath — supporting the tangled_rope/high-extraction profile authored here. If they are not separable, the ''doctrine unchanged'' premise itself may be a retrospective narrative device, which would push this reading toward the hybrid_pragmatic_reading''s structure instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_practice_separability, conceptual, 'Whether the doctrine/practice split this reading depends on is a coherent structural fact or an interpretive convenience.').

omega_variable(
    membership_legitimacy_crisis_scope,
    'How widely was the gap between material suspension and spiritual continuity actually perceived by ordinary Church membership at the time, versus retrospectively by historians?',
    'Diaries, ward records, and correspondence of practicing plural-marriage families in 1890-1904 documenting whether they experienced the Manifesto as coerced betrayal, continued private obligation, or accepted revelation.',
    'If the legitimacy crisis was widely felt contemporaneously, the victim-bearing-doctrinal-abandonment-costs structure authored here is well-evidenced. If it is largely a retrospective historiographical framing, the extractiveness score may overstate contemporaneous suppression relative to later normalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_legitimacy_crisis_scope, empirical, 'Whether the legitimacy crisis this reading centers was a lived contemporaneous experience or a later historical reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(marr_tr_t4, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 4, 0.42).
narrative_ontology:measurement(marr_tr_t8, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 8, 0.58).
narrative_ontology:measurement(marr_tr_t14, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 14, 0.7).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 20, 0.66).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(marr_be_t4, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(marr_be_t8, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 8, 0.79).
narrative_ontology:measurement(marr_be_t14, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 14, 0.85).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 30, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(marr_su_t4, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(marr_su_t8, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 8, 0.88).
narrative_ontology:measurement(marr_su_t14, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 14, 0.79).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 30, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the 1890 Manifesto' per the ε-invariance principle: endogenous_reinterpretation_reading treats the Manifesto as genuine revelation (low extraction, no exogenous victim); this exogenous_override_reading treats it as coerced capitulation (high extraction, federal government as beneficiary, practicing membership as victim); hybrid_pragmatic_reading treats it as strategic ambiguity-management (moderate extraction distributed differently, with the Church as a partial co-beneficiary of the ambiguity itself). Each reading is authored with its own stable ε and its own stakeholder structure; they are linked here rather than merged because measuring 'the Manifesto' by different observables (theological self-report vs. federal legislative record vs. institutional strategy analysis) yields materially different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
