% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__ishmael_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__ishmael_covenant_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: abrahamic_covenant__ishmael_covenant_reading
 *   human_readable: Ishmaelite Covenant Reading: Inclusive Abrahamic Succession Through Muhammad
 *   domain: religious/theological/institutional_authority
 *
 * SUMMARY:
 *   This constraint models the Islamic reading of the Abrahamic covenant
 *   kernel: that Genesis's promise to Abraham's descendants is inclusive of
 *   Ishmael's line, culminating in Muhammad's prophethood as a legitimate
 *   continuation of Abrahamic revelation rather than an illegitimate rupture
 *   from it. This is one of three sibling readings of the shared kernel (the
 *   others being the Isaac-exclusive reading and the Christian
 *   supersessionist reading), each instantiating a structurally distinct
 *   constraint with its own beneficiary/victim structure and its own epsilon.
 *   This story authors ONLY the Ishmael-inclusive reading; the sibling
 *   readings are separate constraint files linked via
 *   network.affects_constraints, and no attempt is made here to average or
 *   reconcile epsilon across readings.
 *
 * KEY AGENTS:
 *   - islamic_ummah: primary beneficiary (organized/identity_locked) — receives covenantal legitimacy and religious standing
 *   - islamic_clergy_and_scholars: agenda_setter and beneficiary (institutional/identity_locked) — codify and defend the reading, professional identity depends on it
 *   - jewish_covenantal_exclusivity_claims: payer (organized/trapped) — the exclusivity claim is structurally contested and diluted by the competing reading
 *   - isaac_lineage_communities: payer (organized/trapped) — bear the cost of a rival legitimacy claim relativizing their unique covenantal standing
 *   - christian_typological_readers: excluded (organized/constrained) — hold a third, unaddressed reading, not consulted in this specific two-party contest
 *   - comparative_theologians: analytical observer — study the contest without institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.42).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.38).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Ishmaelite Covenant Reading: Inclusive Abrahamic Succession Through Muhammad").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious/theological/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, 'cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601').
narrative_ontology:cs_kernel_codification('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', fixed_text).
narrative_ontology:cs_authority_grounding('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', lineage).
narrative_ontology:cs_interpretation_layer_present('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601').
narrative_ontology:cs_reading_relation('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_axiom('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', foundational, covenant_transmission_admits_non_lineal_branching).
narrative_ontology:cs_axiom_status(covenant_transmission_admits_non_lineal_branching, holdable).
narrative_ontology:cs_axiom_grounding('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', covenant_transmission_admits_non_lineal_branching, theological).
narrative_ontology:cs_axiom('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', secondary, genesis_exclusion_clause_reads_as_particular_not_universal_negation).
narrative_ontology:cs_axiom_status(genesis_exclusion_clause_reads_as_particular_not_universal_negation, holdable).
narrative_ontology:cs_axiom_grounding('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', genesis_exclusion_clause_reads_as_particular_not_universal_negation, conventional).
narrative_ontology:cs_reference_frame('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', quranic_self_authenticating_prophethood_with_abrahamic_continuity).
narrative_ontology:cs_drift_state('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', contemporary_interfaith_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('cff3fd04-9a0b-4c5e-a0d0-9a6b7826c601', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_ummah).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_clergy_and_scholars).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_prophetic_authority_claims).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, jewish_covenantal_exclusivity_claims).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, isaac_lineage_communities).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, genesis_promise_universalizability).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, prophetic_succession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives standing as the legitimate continuation of Abrahamic covenant through Ishmael and Muhammad. This reading grounds the community's religious identity, legal legitimacy (as heirs to prophetic revelation), and place within a shared Abrahamic narrative that would otherwise exclude them. Exit from the reading would mean exit from a core self-understanding, not merely a doctrinal preference.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_ummah, beneficiary,
    organized, civilizational, identity_locked, global).

% Teach, codify, and defend the inclusive covenant reading through tafsir, hadith scholarship, and institutional religious authority. Their professional and institutional legitimacy is built on maintaining this interpretive tradition; abandoning it would dissolve the theological basis of their office.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_clergy_and_scholars, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__ishmael_covenant_reading, islamic_clergy_and_scholars, beneficiary).

% The Isaac-exclusive reading (Genesis 17:19-21, read as excluding Ishmael) is the standing textual basis for a distinct covenantal identity. The Ishmael-inclusive reading does not physically coerce anyone, but it structurally contests and dilutes the exclusivity claim by asserting a rival, textually-grounded successor line — the contest cannot be exited from within either tradition's own framework.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, jewish_covenantal_exclusivity_claims, payer,
    organized, civilizational, trapped, global).

% Communities whose religious self-understanding rests on Isaac being the sole covenantal heir bear the cost of a competing legitimacy claim that, if widely accepted, would relativize their unique standing. They cannot simply exit the interpretive contest since it is constitutive of their tradition's founding claim.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, isaac_lineage_communities, payer,
    organized, civilizational, trapped, global).

% Hold a third reading (supersessionist, fulfilled-in-Christ) that is neither validated nor directly addressed by the Ishmael reading. They are not parties to this specific contest but are affected by any resolution of covenant-transmission questions and are not consulted in either Jewish or Islamic framings.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, christian_typological_readers, excluded,
    organized, civilizational, constrained, global).

% Study the textual, historical, and institutional dimensions of competing covenant readings without institutional stake in any single outcome; document how each tradition's authority structure processes the same base text differently.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, comparative_theologians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading solves the problem of religious legitimacy and continuity for a large, newly-formed community (7th century Arabia and beyond) by grounding its prophetic claims in a pre-existing, widely-recognized Abrahamic textual tradition rather than requiring an entirely novel revelatory basis.
% TRANSFER_FUNCTION: Moves interpretive authority and covenantal standing from an exclusive claim (Isaac's line alone) to an expanded claim (Ishmael's line included), transferring symbolic legitimacy capital from Jewish exclusivity traditions toward Islamic institutional authority without any physical resource transfer.
% ABSENT_VOICES: Christian supersessionist readers, who hold their own distinct claim on Abrahamic succession through spiritual (not lineal) fulfillment, are not addressed by this reading and are not present in the Jewish-Islamic contest this constraint concerns; their absence means the two-party framing of 'exclusive vs. inclusive' understates the full field of contested claims.
% DISAPPEARANCE_RATIONALE: Islamic theology holds that Muhammad's prophetic legitimacy does not depend solely on this genealogical reading — the Quran is held as self-authenticating revelation independent of Abrahamic lineage arguments. Jewish and comparative-theology observers would likely say the disappearance of this specific interpretive claim would meaningfully alter interfaith polemic and apologetics but would not dissolve Islamic religious practice itself. The parties dispute how load-bearing the reading actually is.
% FOUNDING_PROBLEM: How does a new prophetic movement establish legitimacy and continuity with an already-authoritative textual and religious tradition (the Abrahamic scriptures) that predates it and whose adherents would otherwise regard it as illegitimate innovation?
% FOUNDING_PROBLEM_CORROBORATION: Islamic scholars and the ummah attest the problem remains live as an ongoing apologetic and identity function. Outside corroboration is harder to find without bias: Jewish and Christian theologians who engage comparative Abrahamic scholarship (e.g., in academic religious studies departments) attest that the genealogical argument continues to function polemically in interfaith contexts, though they dispute its textual warrant. No fully disinterested third party exists, since virtually all interpreters of Genesis 17 have a stake in one of the three traditions; this is stated plainly as a limit on corroboration.
narrative_ontology:disappearance_verdict(abrahamic_covenant__ishmael_covenant_reading, contested).
narrative_ontology:founding_problem_status(abrahamic_covenant__ishmael_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__ishmael_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__ishmael_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__ishmael_covenant_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__ishmael_covenant_reading_tests).
:- end_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the reading does not transfer material resources or coerce practice from the Isaac-lineage or Jewish exclusivity claims — it operates entirely at the level of symbolic and interpretive legitimacy, which nonetheless has real institutional consequences (interfaith polemic, apologetic literature, historical religious conflict framing). Suppression is moderate (0.38): the reading is defended through institutional teaching, scholarly consensus-building, and doctrinal authority rather than physical coercion, but it does foreclose certain interpretive moves for those inside Islamic tradition (the reading is not optional within orthodox Islamic theology). Theater ratio rose slowly over the interval (0.15 to 0.28) as the doctrinal claim moved from an early, functionally load-bearing legitimacy argument toward a more ritualized/apologetic function once Islamic institutional authority was independently well-established — the claim persists more from tradition-maintenance than from active legitimacy-founding necessity in later centuries. Accessibility collapse is moderate (0.45): alternative readings of Genesis remain fully available and are actively held by billions of adherents of other traditions — this is not a mountain-grade collapse of alternatives. Resistance is notably high (0.62) because this is precisely the kind of claim that meets sustained, organized, textually-grounded counter-argument from Jewish and Christian scholarly traditions.
 *
 * DIRECTIONALITY LOGIC:
 *   Islamic ummah and clergy sit near the beneficiary end: the reading is load-bearing for their communal identity and institutional authority, and they experience essentially no cost from it (d low). Jewish covenantal-exclusivity claims and Isaac-lineage communities sit toward the target end: they bear a real, if symbolic, cost in the form of a contested and diluted claim to unique covenantal status, and they cannot exit the theological contest since it engages the founding text of their own tradition (d moderate-high, tempered by the fact that no physical coercion is present — this is a battle of texts and institutions, not of material extraction). Both payer groups are marked exit_options: trapped not because of physical confinement but because the contest is internal to a shared textual inheritance neither side can simply walk away from without abandoning their own scripture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing legitimate continuity between a new prophetic movement and an authoritative prior tradition — was genuinely live at the reading's origin (7th century). Its status today is contested rather than dead: Islamic theology holds the claim continues to do real interpretive and apologetic work, while outside observers note Islamic religious practice does not structurally depend on this specific genealogical argument (the Quran's self-authenticating claims are independently sufficient within Islamic doctrine). This is not classified as mandatrophy because the coordination function (grounding a large religious community's relationship to a shared textual inheritance) remains actively invoked, not merely inertially maintained — though the rising theater_ratio suggests some drift toward performative/apologetic use over pure identity-grounding use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ishmael_reading_vs_isaac_reading_foreclosure,
    'Does the Ishmael-inclusive reading logically foreclose the Isaac-exclusive reading within a single interpretive framework, or can both be held as coexisting claims by different faith communities without internal contradiction?',
    'Textual-critical analysis of Genesis 17:19-21 and comparative analysis of how each tradition''s own canonical exegesis treats the exclusion clause; examine whether any single tradition''s own scholars have historically held both readings simultaneously without contradiction.',
    'If the readings genuinely foreclose one another only within a shared single framework (which does not exist, since Jewish and Islamic traditions operate independent canonical frameworks), the coexists_with classification is correct and no single arbiter can resolve the contest. If a shared meta-framework (e.g. historical-critical biblical scholarship) is treated as authoritative by both sides, the readings could be forced into direct logical conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ishmael_reading_vs_isaac_reading_foreclosure, conceptual, 'Whether the Ishmael and Isaac readings are structurally exclusive or merely rival within independent frameworks.').

omega_variable(
    genealogical_argument_load_bearing_status,
    'How load-bearing is the Ishmael-Muhammad genealogical continuity argument for Islamic theological legitimacy today, versus in the 7th-9th century formative period?',
    'Comparative analysis of classical vs. contemporary Islamic apologetic literature; survey of contemporary Islamic theological curricula to assess whether the genealogical argument is taught as central or as one supporting strand among several independent legitimacy claims (Quranic self-authentication, hadith authentication, historical transmission chains).',
    'If the argument was heavily load-bearing early and is now largely apologetic/theater, this supports the rising theater_ratio trajectory and a mild mandatrophy-adjacent reading (function migrated from identity-founding to identity-defending). If it remains equally load-bearing, the flat-to-rising extractiveness trajectory should be reconsidered as understating persistent function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genealogical_argument_load_bearing_status, empirical, 'Whether the covenant-continuity claim''s functional weight has shifted over the historical interval.').

omega_variable(
    committer_framing_kernel_vs_reading_boundary,
    'Is the disagreement between the Ishmael and Isaac readings located in the interpretation of the Genesis text itself, or in a prior, unstated disagreement about whether prophetic succession can occur outside a single bloodline at all?',
    'Trace each tradition''s hermeneutical principles for covenant transmission (lineal vs. spiritual/functional criteria) independently of the Ishmael/Isaac specific dispute, to locate whether the disagreement is textual (what does Genesis 17 say) or doctrinal-prior (what counts as valid succession).',
    'If the disagreement is prior/doctrinal rather than textual, then no amount of textual-critical resolution of Genesis 17 could settle the contest, and the two readings are more accurately described as expressions of independently-formed doctrinal commitments about succession criteria rather than competing exegeses of a shared passage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_kernel_vs_reading_boundary, conceptual, 'Where the reading disagreement is actually located: in the text or in prior doctrinal commitments about succession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(abra_tr_t0, observed).
narrative_ontology:measurement(abra_tr_t230, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 230, 0.18).
narrative_ontology:measurement_basis(abra_tr_t230, observed).
narrative_ontology:measurement(abra_tr_t470, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 470, 0.2).
narrative_ontology:measurement_basis(abra_tr_t470, observed).
narrative_ontology:measurement(abra_tr_t700, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 700, 0.23).
narrative_ontology:measurement_basis(abra_tr_t700, observed).
narrative_ontology:measurement(abra_tr_t930, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 930, 0.25).
narrative_ontology:measurement_basis(abra_tr_t930, observed).
narrative_ontology:measurement(abra_tr_t1160, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1160, 0.27).
narrative_ontology:measurement_basis(abra_tr_t1160, observed).
narrative_ontology:measurement(abra_tr_t1400, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1400, 0.28).
narrative_ontology:measurement_basis(abra_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(abra_be_t0, observed).
narrative_ontology:measurement(abra_be_t230, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 230, 0.35).
narrative_ontology:measurement_basis(abra_be_t230, observed).
narrative_ontology:measurement(abra_be_t470, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 470, 0.38).
narrative_ontology:measurement_basis(abra_be_t470, observed).
narrative_ontology:measurement(abra_be_t700, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 700, 0.4).
narrative_ontology:measurement_basis(abra_be_t700, observed).
narrative_ontology:measurement(abra_be_t930, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 930, 0.41).
narrative_ontology:measurement_basis(abra_be_t930, observed).
narrative_ontology:measurement(abra_be_t1160, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1160, 0.42).
narrative_ontology:measurement_basis(abra_be_t1160, observed).
narrative_ontology:measurement(abra_be_t1400, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1400, 0.42).
narrative_ontology:measurement_basis(abra_be_t1400, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(abrahamic_covenant__ishmael_covenant_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__ishmael_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the abrahamic_covenant kernel (isaac_covenant_reading, ishmael_covenant_reading, christian_supersessionist_reading), each a structurally distinct constraint with its own beneficiary/victim set and epsilon, per the epsilon-invariance decomposition principle. It also structurally interacts with land_promise_constraint, since covenant-transmission readings are frequently invoked alongside (but are analytically distinct from) territorial-grant readings of the same Genesis narrative; a party's stance on lineal succession does not determine, but does correlate with, their stance on the land promise's conditionality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
