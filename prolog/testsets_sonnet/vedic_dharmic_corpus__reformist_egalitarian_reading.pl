% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist-Egalitarian Reading of the Vedic-Dharmic Corpus
 *   domain: religious authority/social stratification/interpretive legitimacy
 *
 * SUMMARY:
 *   This story instantiates the reformist-egalitarian reading of the
 *   Vedic-dharmic corpus: the claim that textual meaning must be brought into
 *   conformity with constitutional equality principles, that caste hierarchy
 *   represents historical accretion rather than scriptural essence, and that
 *   rational critique of tradition supersedes inherited authority. This
 *   reading is one of three structurally distinct constraints sharing the
 *   same kernel (the Vedic-dharmic corpus as a stabilized textual/traditional
 *   commitment). The hereditary-monopoly reading holds the opposite factual
 *   claim about the same texts (hierarchy is essential, not accretive) and is
 *   a separate story with an inverted beneficiary structure. The
 *   bhakti-devotional reading bypasses the hierarchy question entirely
 *   through a different mechanism (devotional access) and is also a separate
 *   story. This story's epsilon (~0.45) reflects genuine coordination value
 *   (a shared framework letting state and reform movements act jointly)
 *   combined with real extraction from orthodox institutions and hereditary
 *   lineages whose customary authority is displaced by legal enforcement of
 *   the reading.
 *
 * KEY AGENTS:
 *   - dalit_rights_movements: primary beneficiary (organized/constrained) — uses the reading to press material claims
 *   - constitutional_state_apparatus: agenda_setter (institutional/analytical) — administers enforcement, could decline but at high legitimacy cost
 *   - orthodox_temple_institutions: primary payer (powerful/constrained) — loses customary control
 *   - hereditary_priestly_lineages: primary payer (moderate/constrained) — loses birth-based occupational monopoly
 *   - reformist_intelligentsia: secondary beneficiary (moderate/mobile) — produces the doctrine, has more exit than movements dependent on its material payoff
 *   - comparative_religion_scholars: analytical observer — documents construction of all three readings without a stake in which prevails legally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.38).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist-Egalitarian Reading of the Vedic-Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious authority/social stratification/interpretive legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, 'd3ad3027-6d4b-4c09-8845-ee4f9fea9459').
narrative_ontology:cs_kernel_codification('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', fixed_text).
narrative_ontology:cs_authority_grounding('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', extraction).
narrative_ontology:cs_interpretation_layer_present('d3ad3027-6d4b-4c09-8845-ee4f9fea9459').
narrative_ontology:cs_reading_relation('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', foundational, constitutional_equality_supersedes_textual_prescription).
narrative_ontology:cs_axiom_status(constitutional_equality_supersedes_textual_prescription, holdable).
narrative_ontology:cs_axiom_grounding('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', constitutional_equality_supersedes_textual_prescription, conventional).
narrative_ontology:cs_axiom('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', foundational, hierarchy_is_historically_contingent_not_essential).
narrative_ontology:cs_axiom_status(hierarchy_is_historically_contingent_not_essential, holdable).
narrative_ontology:cs_axiom_grounding('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', hierarchy_is_historically_contingent_not_essential, empirically_contingent).
narrative_ontology:cs_reference_frame('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', textual_hierarchy_as_divine_prescription).
narrative_ontology:cs_drift_state('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', post_constitutional_independence_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d3ad3027-6d4b-4c09-8845-ee4f9fea9459', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_rights_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state_apparatus).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_intelligentsia).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_temple_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_priestly_lineages).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, textual_meaning_as_historically_contingent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize politically and legally around the claim that caste hierarchy is a corrupting historical accretion on the textual tradition, not its essence. They use this reading to press for temple entry, priesthood access, and anti-discrimination enforcement. Their exit from the framework is constrained because abandoning the reformist reading would cede interpretive ground back to orthodox custodians; they are structurally committed to the reading's success.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_rights_movements, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_rights_movements, agenda_setter).

% Courts and legislatures adjudicate religious practice against constitutional equality guarantees, using the reformist reading as the interpretive lens that lets constitutional supremacy override claimed scriptural mandate. It administers the enforcement machinery (temple entry statutes, anti-untouchability law, judicial review of religious custom) and could, in principle, decline to intervene — but political and constitutional legitimacy costs of doing so are high.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_state_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).

% Scholars, jurists, and public intellectuals who produce and circulate the historical-accretion argument. They gain professional and public standing from advancing this reading; if it lost ground they could pivot to other scholarly projects, giving them more exit than the movements whose material claims depend on the reading holding.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_intelligentsia, beneficiary,
    moderate, generational, mobile, national).

% Temple trusts and religious bodies whose customary authority over admission, ritual role, and internal governance is overridden when courts apply the reformist reading. They lose control over who may enter, officiate, or inherit priestly office. Their exit options are constrained by the same legal system enforcing the reading against them; litigation and political lobbying are their main levers, not departure.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_temple_institutions, payer,
    powerful, generational, constrained, regional).

% Families whose social and economic position rests on hereditary ritual monopoly. The reformist reading directly delegitimizes the birth-based warrant for their position and, where enforced, opens ritual office to non-hereditary claimants. They cannot easily exit the identity that is being stripped of its exclusive privilege — their whole professional lineage is the thing being reclassified as historical accretion.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_priestly_lineages, payer,
    moderate, biographical, constrained, local).

% Traditionalist commentators who hold that varna prescriptions are textually essential, not accretive, are treated by the reformist reading's proponents as making an already-refuted claim rather than a live counter-interpretation. Their arguments are heard in courts and public debate but the reformist reading's institutional backing means their objections rarely alter outcomes.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_theological_scholarship, excluded,
    moderate, civilizational, trapped, national).

% Study the historical layering of caste-related textual material, the dating of interpolations, and the political history of reform movements, without a stake in which reading prevails legally. They can document how each reading was constructed and by whom.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_rights_movements).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive standard that lets a constitutional state and reform movements act jointly against caste-based exclusion without repudiating the textual tradition outright — reframing objectionable practices as later corruption rather than core doctrine, which lowers the legitimacy cost of legal intervention.
% TRANSFER_FUNCTION: Moves ritual authority, temple governance control, and hereditary occupational privilege away from orthodox priestly institutions and lineages toward legally empowered reform movements and state-administered access rights; also moves interpretive authority over the textual corpus from traditional custodians to courts and reformist scholarship.
% ABSENT_VOICES: Orthodox theological scholars who hold the hereditary-monopoly reading as textually essential are present in litigation and public debate but structurally outmatched — their objections are treated as the position needing refutation rather than a coequal interpretive claim, because the reformist reading has already captured constitutional backing.
% DISAPPEARANCE_RATIONALE: If the reformist-egalitarian reading lost its legal and institutional purchase, temple entry rights, anti-discrimination enforcement against religious custom, and priesthood-access litigation would lose their interpretive warrant; orthodox institutions would regain unchallenged authority over admission and ritual office, and reform movements would need an entirely different legal theory to press the same material claims.
% FOUNDING_PROBLEM: Untouchability and caste-based exclusion from worship, priesthood, and social participation were enforced partly through appeals to scriptural mandate; the founding problem was how to dismantle that exclusion without either abandoning constitutional commitment to religious freedom or accepting that the tradition's core content was irreducibly hierarchical.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars and legal historians outside both the Dalit movements and the orthodox institutions attest that caste-based exclusion from temples and priesthood remains an active, litigated problem, not a historical curiosity — court dockets and continuing temple-entry disputes corroborate that the founding problem has not been resolved, independent of either side's account of scriptural essence.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the reading does real coordination work — it lets constitutional commitments and religious-freedom protections coexist by relocating the objectionable content to 'accretion' rather than 'essence' — while also transferring concrete authority (temple governance, priesthood access, ritual monopoly) away from orthodox institutions through court-enforced reinterpretation. Suppression is moderate (0.38) and rising over the interval as litigation and statute accumulate enforcement precedent; it is not total because orthodox institutions retain political and rhetorical avenues (lobbying, counter-litigation, theological argument) that are heard, even if they increasingly lose. Theater ratio rises modestly (0.2 to 0.4) reflecting a documented pattern where formal legal victories for temple entry are sometimes followed by continued informal exclusion — enforcement on paper outpacing enforcement in practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalit rights movements and the constitutional state apparatus sit near the beneficiary end: the movements gain material access and standing, the state gains a legitimacy-preserving interpretive tool that lets it intervene in religious practice without appearing to attack religion as such. Orthodox temple institutions and hereditary priestly lineages sit near the target end: their customary and birth-based authority is precisely what the reading reclassifies as illegitimate accretion, and the state's enforcement apparatus is the mechanism by which that reclassification acquires teeth. Reformist intelligentsia benefit but with more exit than movements whose land, temple-access, and livelihood claims are directly staked on the reading's institutional survival.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (scriptural warrant used to enforce caste exclusion) remains live by outside corroboration (ongoing litigation, continuing informal exclusion despite formal entry rights), so this is not a case of an arrangement persisting after its function died. Classifying this as tangled_rope rather than pure snare or pure rope avoids two mislabelings: treating it as pure coordination would erase the real transfer of authority away from orthodox institutions and lineages; treating it as pure extraction would erase the genuine coordination function of letting constitutional equality and religious-freedom claims coexist without a wholesale repudiation of the tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accretion_vs_essence_historicity,
    'Is caste hierarchy in the Vedic-dharmic corpus a demonstrable later interpolation/accretion, or is the accretion narrative itself a retrospective reading imposed to serve contemporary constitutional commitments?',
    'Philological dating of specific passages (varna-related verses in the Purusha Sukta, Manusmriti strata, etc.) against independently established textual chronology, cross-checked against comparative religion scholarship not aligned with either legal outcome.',
    'If the accretion claim is philologically robust, this reading''s coordination function (reconciling tradition and equality without wholesale repudiation) is well-grounded. If the accretion claim is itself a constructed political reading with no stronger textual basis than the hereditary reading, the coordination story functions more as convenient cover for what is otherwise a straightforward legal override of religious custom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accretion_vs_essence_historicity, empirical, 'Whether the historical-accretion claim is philologically demonstrable or a legitimating construction.').

omega_variable(
    kernel_reading_relationship_to_siblings,
    'Do the three readings of the vedic_dharmic_corpus kernel (reformist_egalitarian, hereditary_monopoly, bhakti_devotional) genuinely compete for the same interpretive terrain, or do they operate in substantially separate social domains (legal/constitutional, ritual/institutional, personal/devotional) such that ''contest'' overstates their mutual exclusivity?',
    'Empirical mapping of which social contexts (courts, temple administration, individual devotional practice) each reading actually governs, and whether adherents of one reading typically also hold elements of another in non-contested domains.',
    'If the readings operate in largely separate domains, the kernel''s ''contest'' framing may overstate genuine incompatibility for most practitioners, though it remains real and consequential in the specific domains (temple entry, priesthood access) where they do collide.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship_to_siblings, conceptual, 'Whether the three kernel readings are genuinely mutually exclusive or domain-separated.').

omega_variable(
    state_apparatus_capture_risk,
    'Is the constitutional state apparatus a neutral arbiter applying equality principles, or is it itself pursuing a nation-building/homogenization agenda for which the reformist reading is instrumentally useful independent of its truth?',
    'Comparative analysis of state intervention patterns across religious traditions — does the state apply equality-conformity review symmetrically, or disproportionately to practices associated with historically marginalized groups'' own religious variants?',
    'If the state''s application is asymmetric or politically instrumentalized, part of the measured ''coordination function'' may be better characterized as state extraction of legitimacy/control rather than genuine equality enforcement, which would push the classification toward higher extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_apparatus_capture_risk, conceptual, 'Whether state enforcement of the reformist reading is neutral or itself extractive/instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vedi_tr_t14, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(vedi_tr_t28, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 28, 0.3).
narrative_ontology:measurement(vedi_tr_t42, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 42, 0.34).
narrative_ontology:measurement(vedi_tr_t56, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 56, 0.37).
narrative_ontology:measurement(vedi_tr_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 70, 0.4).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vedi_be_t14, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 14, 0.33).
narrative_ontology:measurement(vedi_be_t28, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 28, 0.38).
narrative_ontology:measurement(vedi_be_t42, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 42, 0.41).
narrative_ontology:measurement(vedi_be_t56, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 56, 0.44).
narrative_ontology:measurement(vedi_be_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 70, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(vedi_su_t14, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 14, 0.27).
narrative_ontology:measurement(vedi_su_t28, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 28, 0.31).
narrative_ontology:measurement(vedi_su_t42, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 42, 0.34).
narrative_ontology:measurement(vedi_su_t56, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 56, 0.36).
narrative_ontology:measurement(vedi_su_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 70, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.1).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language concept 'the caste-and-scripture question' into structurally distinct claims per the epsilon-invariance principle. hereditary_monopoly_reading claims scriptural essence for hierarchy and has an inverted beneficiary/victim structure (orthodox institutions benefit, reform movements are excluded/victimized). bhakti_devotional_reading bypasses the hierarchy question via a third mechanism (devotional sincerity as spiritual warrant) and has yet another beneficiary structure. All three share the same kernel_id (vedic_dharmic_corpus) but are not the same constraint — measuring 'the caste question' via constitutional-conformity versus via scriptural-essence versus via devotional-access yields three different epsilon values, which is exactly the signal that these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
