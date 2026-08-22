% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__allegorical_ancient_near_east, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis 1-2 Read as Ancient Near Eastern Mythopoetic Literature (No Historical-Scientific Claims)
 *   domain: religious_studies/biblical_hermeneutics/science_religion_interface
 *
 * SUMMARY:
 *   This constraint models the allegorical/Ancient Near Eastern-mythopoetic
 *   reading of Genesis 1-2 as a single, structurally distinct claim within a
 *   contested interpretive kernel. Under this reading, the text is
 *   genre-classified alongside Enuma Elish and the Atrahasis Epic: a
 *   theological cosmogony using the literary conventions of its ancient
 *   environment, making no historical or scientific claims about cosmological
 *   origins or biological process. This decouples the text completely from
 *   the creation-science and creation-evolution debates and drains the
 *   'dominion' language of any normative force over environmental policy,
 *   since it is read as ANE royal-ideology metaphor rather than a literal
 *   grant of authority. The reading functions as coordination (letting
 *   religious institutions retain scriptural authority while embracing
 *   mainstream science) and as extraction (transferring interpretive
 *   authority to a credentialed guild and marginalizing lay and
 *   traditionalist readings as pre-critical), which is why it is authored as
 *   tangled_rope rather than a pure rope or pure snare.
 *
 * KEY AGENTS:
 *   - critical_biblical_scholars: agenda-setting guild that administers the interpretive standard
 *   - progressive_theological_institutions: institutional beneficiaries of cultural legitimacy
 *   - lay_congregants_seeking_certainty: bear the cost of lost interpretive ground
 *   - traditionalist_denominational_minorities: marginalized by the reading's institutional dominance
 *   - biblical_literalist_communities: organized resistance bearing reputational and institutional cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.42).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.38).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.42).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 Read as Ancient Near Eastern Mythopoetic Literature (No Historical-Scientific Claims)").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics/science_religion_interface").

domain_priors:requires_active_enforcement(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, 'e4bc0f96-e648-4aa5-835c-c8bb1186aab7').
narrative_ontology:cs_kernel_codification('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', fixed_text).
narrative_ontology:cs_authority_grounding('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', expertise).
narrative_ontology:cs_interpretation_layer_present('e4bc0f96-e648-4aa5-835c-c8bb1186aab7').
narrative_ontology:cs_reading_relation('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', genesis_creation_narrative__theistic_evolutionary, influences).
narrative_ontology:cs_axiom('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', foundational, genre_determines_referential_scope).
narrative_ontology:cs_axiom_status(genre_determines_referential_scope, holdable).
narrative_ontology:cs_axiom_grounding('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', genre_determines_referential_scope, conventional).
narrative_ontology:cs_axiom('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', foundational, text_makes_no_cosmological_or_biological_claims).
narrative_ontology:cs_axiom_status(text_makes_no_cosmological_or_biological_claims, holdable).
narrative_ontology:cs_axiom_grounding('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', text_makes_no_cosmological_or_biological_claims, empirically_contingent).
narrative_ontology:cs_axiom('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', secondary, dominion_language_is_ane_royal_metaphor_not_mandate).
narrative_ontology:cs_axiom_status(dominion_language_is_ane_royal_metaphor_not_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', dominion_language_is_ane_royal_metaphor_not_mandate, conventional).
narrative_ontology:cs_reference_frame('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', pre_critical_historical_chronicle_reading).
narrative_ontology:cs_drift_state('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', post_higher_criticism_and_ane_comparative_philology_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('e4bc0f96-e648-4aa5-835c-c8bb1186aab7', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, progressive_theological_institutions).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_engaged_clergy).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, lay_congregants_seeking_certainty).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, traditionalist_denominational_minorities).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, biblical_literalist_communities).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, ane_comparative_literature_method).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, genre_sensitive_hermeneutics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the interpretive standard in seminary curricula, academic journals, and critical commentaries by reading Genesis 1-2 against Enuma Elish, the Atrahasis Epic, and other ANE cosmogonies. Their professional standing, publication record, and institutional authority depend on this comparative-genre method being treated as the responsible default. They can move between denominational and secular academic settings freely.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars, beneficiary).

% Seminaries and denominational bodies that adopt this reading gain credibility with secular academia and avoid public conflict with mainstream science education, protecting enrollment and cultural standing. They benefit from the decoupling without bearing the cost of congregational backlash directly.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, progressive_theological_institutions, beneficiary,
    organized, generational, mobile, national).

% Pastors and teachers who want to affirm both faith and mainstream science use this reading to resolve pulpit-level tension. It lets them avoid choosing between congregants who accept evolution and the authority of scripture, but they risk congregational division if they push the reading too visibly.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, science_engaged_clergy, beneficiary,
    moderate, biographical, constrained, regional).

% Ordinary believers who were taught the text as historical narrative and relied on it for a stable cosmological and moral anchor. When told the account is myth-genre and carries no adjudicative claim about origins, they experience this as the ground shifting under a text they were told was foundational. Their exit options are limited to switching congregations or abandoning the text's authority altogether, both costly to identity and community.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, lay_congregants_seeking_certainty, payer,
    powerless, biographical, constrained, local).

% Smaller conservative denominations and sects whose entire theological structure rests on Genesis as historical account. The allegorical reading, when it becomes the academic and cultural default, marginalizes their tradition as pre-critical or uneducated, reducing their institutional legitimacy, seminary accreditation options, and cultural standing without offering them any practical route to contest the reading on its own methodological terms.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, traditionalist_denominational_minorities, payer,
    powerless, generational, trapped, national).

% Organized literalist movements (young-earth creationist institutions, associated publishing and education networks) lose ground as the allegorical reading becomes institutionally dominant in mainstream biblical studies, textbook adoption, and museum/education funding decisions. They retain resources to build parallel institutions but are pushed to the cultural margins of the guild.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, biblical_literalist_communities, payer,
    organized, generational, constrained, national).

% The comparative corpus of Mesopotamian, Ugaritic, and Egyptian cosmogonic texts that supplies the genre-comparison evidence this reading depends on. Not an agent; included for completeness as the evidentiary substrate the method treats as authoritative.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, ane_textual_corpus, observer,
    analytical, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(genesis_creation_narrative__allegorical_ancient_near_east, ane_textual_corpus).

% Would generally welcome this reading as removing a source of manufactured conflict, but has no seat in the intra-religious dispute over which reading a given community should adopt — the reading is decided within religious institutions, not by scientific consensus, even though this reading claims alignment with it.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, secular_scientific_community, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive frame that lets religious communities affirm the text's theological content (created order, human dignity, divine intent) while avoiding direct collision with cosmology and evolutionary biology — solving the genuine coordination problem of maintaining both scriptural commitment and participation in modern scientific culture.
% TRANSFER_FUNCTION: Moves interpretive authority away from lay tradition and congregational consensus and toward the credentialed guild of critical biblical scholars and the institutions that employ the ANE-comparative method; moves cultural legitimacy away from literalist and traditionalist communities toward institutions that adopt the allegorical reading as respectable default.
% ABSENT_VOICES: Lay believers who were catechized into a historical reading are rarely consulted before their tradition's institutions shift teaching; traditionalist minority denominations are typically described by the guild rather than invited into the methodological conversation that determines whether their reading counts as scholarship or as naivety.
% DISAPPEARANCE_RATIONALE: If this reading vanished from mainstream theological education overnight, seminaries would lose a settled way to avoid the creation-science conflict and would have to renegotiate curricula; literalist communities would experience it as vindication rather than loss, while progressive institutions would face renewed public pressure to explain their relationship to scientific cosmology. Whether the world 'rearranges' depends heavily on which seat is asked — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century historical-critical scholarship discovered close parallels between Genesis 1-2 and older Mesopotamian cosmogonies, and geology/evolutionary biology made a strictly historical reading increasingly difficult to sustain without rejecting mainstream science; the allegorical ANE reading was built to let biblical scholarship and theology survive this evidentiary pressure without abandoning the text's authority altogether.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of science and religion (documenting the nineteenth-century geology and Darwinian controversies) and secular ANE philologists outside any theological institution corroborate that the comparative evidence and the scientific pressure are real and ongoing; this corroboration comes from outside the community of scholars who benefit professionally from the reading's adoption, though the *theological* verdict that this specific reading is the correct resolution remains internal to religious institutions and is not itself externally corroborated.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, contested).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).
:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: the reading provides genuine coordination value (avoiding manufactured science-religion conflict) alongside real transfer of interpretive authority and cultural legitimacy away from lay and traditionalist readers. Suppression is moderate (0.38) because the mechanism is largely institutional and reputational (accreditation, curriculum adoption, publishing gatekeeping) rather than coercive; it does not physically prevent anyone from holding the literalist reading, but it does impose real professional and social costs on those who dissent from the guild consensus. Theater ratio is modest (0.3) — the comparative ANE scholarship is substantively real work, not mere performance, though some institutional adoption is more about signaling academic respectability than deep engagement with the texts.
 *
 * PERSPECTIVAL GAP:
 *   From the critical-scholar seat, this reading is straightforwardly correct historical-critical method with no extraction at all — simply reading ancient literature according to its own genre conventions. From the traditionalist and literalist seats, the same reading operates as an imposed reclassification that strips their tradition's founding text of the authority they were taught it carries, using academic prestige as the enforcement mechanism rather than force. The engine should compute a lower effective extraction for the agenda-setter seat and a substantially higher one for the powerless, trapped traditionalist seat under the same base ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical scholars and progressive institutions are declared beneficiaries because the reading's adoption directly enhances their institutional standing and resolves the tension they are most professionally exposed to; their d sits near the beneficiary end. Lay congregants and traditionalist minorities are declared victims because the reading's dominance directly displaces the authority structure their faith communities were built around, with limited exit (trapped or constrained) amplifying their effective extraction. Biblical literalist communities, while organized and resourced, are also victims of the reading's institutional dominance but retain enough organizational capacity to build parallel institutions, which is why their exit is coded constrained rather than trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling scriptural authority with historical-critical and scientific evidence) is live, not dead — the underlying tension between ANE comparative philology, geological/evolutionary evidence, and traditional readings has not resolved or gone away. This blocks a simple mandatrophy verdict: the arrangement is not persisting past its function on inertia alone. What keeps this from being pure coordination is that the guild's continued authority over the reading generates ongoing extraction (marginalization of dissenting readings) independent of whether the founding problem remains live — a genuinely resolved coordination function would not require continuously suppressing rival readings as unscholarly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genre_classification_authority,
    'Who has legitimate authority to determine the literary genre of an ancient religious text — the academic guild practicing comparative philology, the religious tradition''s own transmitted self-understanding, or some negotiated combination — and does genre classification itself carry theological stakes that outrun purely literary-critical method?',
    'This is not fully empirically resolvable: it depends on contested views about the relationship between historical-critical method and religious authority. Partial resolution could come from tracing whether the earliest transmission communities themselves treated the text as historical chronicle or as cosmogonic myth, insofar as that evidence is recoverable.',
    'If the genre-classification authority properly belongs to the academic guild alone, this reading is closer to pure coordination (correcting a genre error). If genre classification is itself a site of theological contest that the guild cannot neutrally adjudicate, this reading''s institutional dominance functions more clearly as extraction of interpretive authority from tradition-internal readers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genre_classification_authority, conceptual, 'Whether genre classification of Genesis is a neutral scholarly finding or a contested site of interpretive authority.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (allegorical_ancient_near_east) of the contested Genesis 1-2 creation-narrative kernel; two sibling readings exist as separate constraints — literal_young_earth and theistic_evolutionary. Where exactly is the disagreement between these readings located: is it a disagreement about historical facts (what genre the text originally was), about theological method (how much authority scientific consensus should have over scriptural interpretation), or about institutional politics (which reading protects which community''s authority structure)?',
    'Compare the axioms and reading_relations declared across the three sibling constraint files: literal_young_earth would declare inerrancy/historicity axioms that this reading treats as overridden by genre evidence; theistic_evolutionary would declare a middle-ground axiom compatible with cosmological time but retaining some theological sequencing claims this reading denies entirely.',
    'If the disagreement is purely about historical genre facts, it should in principle be resolvable by philological evidence and the readings should converge over time. If it is substantially about institutional authority and community identity, the readings will likely remain permanently coexisting rather than converging, regardless of further textual evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame omega: locates where the three-way kernel disagreement actually resides (fact, method, or institutional politics), routing the cross-reading structure out of the base fields per Rule 2.').

omega_variable(
    extraction_versus_correction,
    'Is the marginalization of literalist and traditionalist readings under this reading''s institutional dominance better described as extraction (illegitimate transfer of authority to a credentialed guild) or as correction (the appropriate consequence of a reading being shown methodologically inadequate)?',
    'No purely empirical resolution exists; this turns on contested epistemic and theological values about how much deference historical-critical consensus is owed relative to living tradition and lay religious experience.',
    'Framing this as correction would push the classification toward rope (legitimate epistemic coordination with acceptable transitional costs); framing it as extraction supports the tangled_rope classification authored here, where a real coordination function coexists with an asymmetric cost borne by less powerful traditionalist communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_versus_correction, preference, 'Whether displacing traditionalist readings counts as epistemic correction or as extractive marginalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gene_tr_t20, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 20, 0.18).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 40, 0.22).
narrative_ontology:measurement(gene_tr_t60, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 60, 0.25).
narrative_ontology:measurement(gene_tr_t80, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 80, 0.28).
narrative_ontology:measurement(gene_tr_t100, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gene_be_t20, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(gene_be_t40, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 40, 0.33).
narrative_ontology:measurement(gene_be_t60, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 60, 0.37).
narrative_ontology:measurement(gene_be_t80, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(gene_be_t100, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gene_su_t20, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 20, 0.24).
narrative_ontology:measurement(gene_su_t40, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 40, 0.28).
narrative_ontology:measurement(gene_su_t60, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 60, 0.32).
narrative_ontology:measurement(gene_su_t80, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 80, 0.35).
narrative_ontology:measurement(gene_su_t100, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, identity_coordination).
narrative_ontology:boltzmann_floor_override(genesis_creation_narrative__allegorical_ancient_near_east, 0.1).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'the Genesis creation account' per the epsilon-invariance principle: literal_young_earth (claimed mountain/rope by its own adherents, near-total accessibility collapse for insiders, high suppression of dissent within literalist institutions), allegorical_ancient_near_east (this story — tangled_rope, moderate extraction, guild-mediated authority transfer), and theistic_evolutionary (expected tangled_rope or rope, intermediate epsilon, partial decoupling). Each reading has a distinct beneficiary/victim structure and a distinct epsilon; they are linked here via affects_constraints rather than merged into one story with a measurement parameter, per DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
