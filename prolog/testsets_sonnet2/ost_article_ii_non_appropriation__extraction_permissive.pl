% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Outer Space Treaty Article II — Extraction-Permissive Reading (Private Resource Ownership Without Sovereign Claim)
 *   domain: international_law/space_governance/commons
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Article II kernel:
 *   that the treaty's bar on sovereign territorial claims does not extend to
 *   private ownership of resources once extracted. Under this reading, the
 *   silence of the 1967 text on resource title is read as permission rather
 *   than prohibition, and domestic legislation (the US 2015 CSLCA,
 *   Luxembourg's 2017 space resources law) has operationalized it into a
 *   functioning legal-commercial regime. This is not a story about whether
 *   extraction is good or bad in the abstract — it is a story about the
 *   specific structural arrangement this ONE reading produces: resource
 *   access gated by launch capability and flag-state legal recognition, with
 *   no compensation mechanism running back to non-spacefaring parties, and
 *   enclosure achieved by unilateral fait accompli rather than by the formal
 *   annexation the treaty text actually forbids. The commons_conservation and
 *   international_regime readings are separate constraints with their own ε
 *   values and are not represented here except as siblings in the omega
 *   variables and cs_structure fields.
 *
 * KEY AGENTS:
 *   - spacefaring_launch_states: agenda_setter (institutional/arbitrage) — administers the reading via domestic legislation
 *   - private_resource_extraction_firms: beneficiary (organized/mobile) — captures legal certainty and jurisdictional arbitrage
 *   - non_spacefaring_states: payer (powerless/trapped) — bears enclosure cost with no compensation
 *   - copuos_legal_subcommittee: excluded (institutional/analytical) — formal venue structurally outpaced by unilateral action
 *   - treaty_drafters_1967: observer (analytical/analytical) — original intent invoked by all readings, resolves none
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.71).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.42).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.71).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Outer Space Treaty Article II — Extraction-Permissive Reading (Private Resource Ownership Without Sovereign Claim)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_law/space_governance/commons").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, 'd2075192-671b-45ce-b12a-6ef928fed8aa').
narrative_ontology:cs_kernel_codification('d2075192-671b-45ce-b12a-6ef928fed8aa', fixed_text).
narrative_ontology:cs_authority_grounding('d2075192-671b-45ce-b12a-6ef928fed8aa', distributed).
narrative_ontology:cs_reading_relation('d2075192-671b-45ce-b12a-6ef928fed8aa', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('d2075192-671b-45ce-b12a-6ef928fed8aa', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('d2075192-671b-45ce-b12a-6ef928fed8aa', foundational, sovereign_claim_and_private_title_are_severable).
narrative_ontology:cs_axiom_status(sovereign_claim_and_private_title_are_severable, holdable).
narrative_ontology:cs_axiom_grounding('d2075192-671b-45ce-b12a-6ef928fed8aa', sovereign_claim_and_private_title_are_severable, conventional).
narrative_ontology:cs_axiom('d2075192-671b-45ce-b12a-6ef928fed8aa', foundational, treaty_silence_on_extraction_constitutes_permission).
narrative_ontology:cs_axiom_status(treaty_silence_on_extraction_constitutes_permission, holdable).
narrative_ontology:cs_axiom_grounding('d2075192-671b-45ce-b12a-6ef928fed8aa', treaty_silence_on_extraction_constitutes_permission, conventional).
narrative_ontology:cs_reference_frame('d2075192-671b-45ce-b12a-6ef928fed8aa', cold_war_territorial_annexation_prevention_regime).
narrative_ontology:cs_drift_state('d2075192-671b-45ce-b12a-6ef928fed8aa', post_2015_domestic_legislation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2075192-671b-45ce-b12a-6ef928fed8aa', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_launch_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_resource_extraction_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, domestic_space_resource_legislators).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations_of_claimants).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, developing_nations_without_launch_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pass domestic legislation (e.g. asteroid resource property statutes) recognizing private title to extracted space resources while formally disclaiming any territorial sovereignty claim. They administer the reading by drafting and defending the domestic legal architecture that operationalizes it, and by declining to negotiate a binding multilateral resource regime.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_launch_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Depend on the extraction-permissive reading to secure investor confidence and legal title over resources they extract from asteroids or lunar regolith. They lobby for domestic recognition statutes and structure operations through whichever flag state offers the most favorable legal recognition — a form of jurisdictional shopping enabled entirely by the reading's ambiguity.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, private_resource_extraction_firms, beneficiary,
    organized, biographical, mobile, global).

% Draft and pass national statutes (US Commercial Space Launch Competitiveness Act, Luxembourg's space resources law) asserting that Article II's silence on private ownership of extracted resources means such ownership is permitted. They gain geopolitical and economic first-mover advantage for their domestic industries by codifying this reading before any multilateral resolution exists.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, domestic_space_resource_legislators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, domestic_space_resource_legislators, beneficiary).

% Lack the technological capability to extract resources themselves and have no seat at the table where domestic recognition statutes are drafted. Under this reading, the common heritage they were promised under Article I becomes, in practice, first-come property of whichever state can launch and legally recognize extraction. They bear the cost of enclosure without any compensation mechanism and without having contested or ratified the reading that produces it.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states, payer,
    powerless, generational, trapped, global).

% Possess UN voice and formal treaty standing but no practical capacity to extract resources or to enforce an alternative reading against established spacefaring states. Their objections at COPUOS are noted but do not alter the operative legal reality on the ground, which is set by whoever extracts first and whoever's domestic law recognizes the title.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, developing_nations_without_launch_capacity, payer,
    moderate, generational, constrained, global).

% Inherit whatever allocation of easily accessible space resources current extraction-permissive practice produces. They have no representation in current legal proceedings and no mechanism to reclaim resources already extracted and titled under this reading before any future international regime could apply.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations_of_claimants, payer,
    powerless, civilizational, trapped, universal).

% The UN body formally tasked with elaborating space law consensus has been unable to produce a binding multilateral resource regime; its deliberations proceed in parallel to, and are effectively overtaken by, unilateral domestic legislation enacting the extraction-permissive reading. Its consensus-based process structurally cannot outpace unilateral fait accompli.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, copuos_legal_subcommittee, excluded,
    institutional, generational, analytical, global).

% The original negotiators intended Article II to prevent a repeat of terrestrial colonial enclosure; the resource-extraction question was largely unanticipated at the drafting moment, since orbital and lunar resource technology did not yet exist. Their intent is invoked by all three sibling readings as supporting evidence, and settles none of them conclusively.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, treaty_drafters_1967, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, private_resource_extraction_firms).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal certainty for capital-intensive space resource ventures by establishing that extraction is not per se unlawful under the non-appropriation principle, enabling investment and insurance markets to price extraction projects without treaty-violation risk.
% TRANSFER_FUNCTION: Moves de facto control over accessible extraterrestrial resources (water ice, regolith metals, volatile compounds) from the pool of all treaty parties (nominally common heritage) to whichever states possess launch and extraction capability and choose to recognize private title domestically — with no compensation flowing back to excluded parties.
% ABSENT_VOICES: Non-spacefaring and developing states raise objections in COPUOS sessions but are structurally excluded from the venue where the operative decision is actually made — domestic legislatures of spacefaring states acting unilaterally. Future generations, who inherit a depleted or already-enclosed resource base, have no voice in any forum.
% DISAPPEARANCE_RATIONALE: If the extraction-permissive reading were displaced (e.g. by binding adoption of the commons_conservation or international_regime reading), current asteroid mining ventures and domestic resource-recognition statutes would lose their legal foundation overnight; investment would require renegotiated multilateral consent structures, and states currently benefiting from first-mover unilateral legislation would lose that advantage.
% FOUNDING_PROBLEM: In 1967 the founding problem was preventing Cold War superpowers from repeating terrestrial colonial territorial annexation in outer space by formal sovereign claim (flags-and-footprints enclosure).
% FOUNDING_PROBLEM_CORROBORATION: Spacefaring states and their legislators attest the founding problem (sovereign territorial annexation) is fully solved by Article II's bar on claims and that resource ownership is a separate, unaddressed question, permissibly filled by domestic law. Independent international law scholars, COPUOS delegations from non-spacefaring states, and several UN General Assembly statements attest that the founding problem was broader — preventing appropriation in substance, not merely in sovereign form — and that the extraction-permissive reading revives the underlying harm through a different legal mechanism; this corroboration comes from outside the beneficiary set (COPUOS delegates, academic commentators with no extraction industry stake).
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71 by story end because the reading's operation transfers de facto control over a nominally common resource pool to technologically capable states with zero compensation flowing to excluded parties — a textbook asymmetric transfer riding on a genuine coordination function (investment certainty for extraction ventures). Suppression is moderate (0.42) rather than high: there is no active coercive suppression of dissent (non-spacefaring states can and do object at COPUOS), but the practical alternative — a binding multilateral resource regime — has been foreclosed by the pace of unilateral domestic legislation, which functions as passive suppression of the alternative rather than active coercion. Theater ratio is modest (0.28) and rising slowly: some diplomatic performance occurs in COPUOS sessions that all parties know will not bind unilateral action, but the underlying commercial-legal function (title certainty for investors) is real and growing, not primarily performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Spacefaring launch states and the extraction firms they license sit near the full-beneficiary end: they set the interpretive agenda via domestic statute and collect the practical benefit (legal certainty, investment flows, resource access) with no reciprocal obligation. Non-spacefaring states and developing nations without launch capacity sit near the full-target end: they are structurally excluded from the venue where the operative decision is made (domestic legislatures, not COPUOS) and bear the enclosure cost with trapped exit options — there is no alternative treaty regime they can invoke that has force against unilateral domestic law. Future generations are declared as a distinct victim class with civilizational time horizon and trapped exit because resources extracted and titled now cannot be reclaimed even if a future regime displaces this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing sovereign territorial annexation) is genuinely dead in the narrow sense the extraction-permissive reading emphasizes — no state has planted a flag and claimed sovereignty since 1967, and Article II's literal bar on that specific act holds. But the founding_problem_status is authored as contested, not dead, because independent corroboration from outside the beneficiary set (COPUOS delegates, academic international-law commentary) holds that the underlying mischief the treaty targeted — enclosure of common heritage without consent of the excluded — is alive and being achieved through domestic-law resource titling instead of sovereign claim. The mismatch between 'problem declared dead by beneficiaries' and 'world rearranges if the reading were displaced' is exactly the zombie/capture signal the R5 interview is designed to surface: a reading that declares its founding problem solved while the substantive harm it was meant to prevent continues under a different legal label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    silence_as_permission_vs_gap,
    'Does Article II''s textual silence on private resource ownership constitute affirmative legal permission (this reading''s premise), or does it constitute an unresolved gap that requires future multilateral closure (the international_regime sibling''s premise)?',
    'A binding ICJ advisory opinion, a widely ratified multilateral resource treaty, or near-universal state practice consistently treating extraction as either permitted or requiring further authorization would resolve which reading has prevailed. Currently only a handful of spacefaring states have acted on the permissive reading; near-universal silence from the remaining ~90 treaty parties leaves the question genuinely open.',
    'If the gap reading prevails, extraction under domestic legislation to date would be legally provisional rather than settled, potentially requiring retroactive negotiation or compensation — collapsing this story''s beneficiary structure. If the permission reading is vindicated by broad subsequent practice, the tangled_rope classification here would likely stabilize into a rope (coordination without contested extraction) as the victim class''s objections lose practical force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silence_as_permission_vs_gap, conceptual, 'Whether treaty silence functions as permission or as an unresolved gap — the central interpretive fork separating this reading from the international_regime sibling.').

omega_variable(
    use_occupation_scope_ambiguity,
    'Does Article II''s phrase ''national appropriation by ... use or occupation'' extend the non-appropriation bar to private extractive use (the commons_conservation sibling''s premise), or does it apply only to the specific act of claiming sovereign territorial title (this reading''s premise)?',
    'Close textual and drafting-history analysis of the 1967 negotiating record, cross-referenced against subsequent state practice and any judicial or arbitral interpretation of ''use or occupation'' in comparable treaty contexts.',
    'If ''use or occupation'' is held to cover extraction, this reading''s core structural claim collapses and the constraint reclassifies toward the commons_conservation sibling''s structure, with extraction becoming presumptively unlawful absent multilateral authorization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_occupation_scope_ambiguity, conceptual, 'The specific textual scope dispute at the center of the kernel contest, located in the phrase ''use or occupation.''').

omega_variable(
    unilateral_legislation_as_customary_law,
    'Does the accumulation of domestic space-resource statutes (US, Luxembourg, UAE, Japan) by a small number of spacefaring states constitute emerging customary international law recognizing the extraction-permissive reading, or does it constitute unlawful unilateral fait accompli that a future binding regime could reverse?',
    'Track whether non-spacefaring states'' formal protests (opinio juris contra) persist or subside over the coming decade, and whether any binding multilateral instrument is adopted that either ratifies or overrides the unilateral statutes.',
    'If customary law crystallizes in favor of this reading, victim compensation claims by excluded states become legally foreclosed; if a binding regime later overrides it, current extraction activity may face retroactive liability, sharply raising this constraint''s suppression and extractiveness metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unilateral_legislation_as_customary_law, empirical, 'Whether unilateral domestic legislation is crystallizing into customary law or remains a contested fait accompli.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2025, 0.25).
narrative_ontology:measurement(ost__tr_t2030, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2030, 0.27).
narrative_ontology:measurement(ost__tr_t2035, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2035, 0.28).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2025, 0.64).
narrative_ontology:measurement(ost__be_t2030, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(ost__be_t2035, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2035, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2020, 0.34).
narrative_ontology:measurement(ost__su_t2025, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement(ost__su_t2030, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2030, 0.4).
narrative_ontology:measurement(ost__su_t2035, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2035, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__extraction_permissive, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the ost_article_ii_non_appropriation kernel, each authored as a separate ε-invariant constraint per the decomposition principle. commons_conservation reads Article II's 'use or occupation' language as covering private extractive appropriation (low ε, coordination-dominant, near-Rope or Mountain-adjacent depending on enforcement data authored there). international_regime treats the appropriation question as deferred to a future multilateral framework, with neither this reading nor commons_conservation authoritative absent that framework (moderate ε, procedurally-focused). This extraction_permissive reading authors the highest ε of the three, reflecting the structural fact that unilateral domestic legislation has already produced operative extraction activity and asymmetric benefit capture in advance of any multilateral resolution — the ε-invariance principle requires this divergence to be modeled as three distinct constraints rather than one constraint with an ambiguous or averaged extraction value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
