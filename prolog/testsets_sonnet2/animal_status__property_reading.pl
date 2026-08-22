% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animal-as-Property Legal Standing (Property Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates the property reading of the animal-status kernel:
 *   animals are legal objects incapable of holding independent rights or
 *   interests cognizable in their own name; ownership is unrestricted except
 *   where welfare statutes carve out specific, human-legislated exceptions
 *   (anti-cruelty law, humane slaughter and transport rules). Under this
 *   reading the constraint's operation is almost entirely a matter of
 *   human-to-human property law — allocating, transferring, and adjudicating
 *   claims over animals as assets. No animal appears in the victim set
 *   because the reading's own premise is that animals cannot be a party to
 *   the relationship it governs. This is deliberately a narrow, ε-invariant
 *   reading: the welfare reading and the abolitionist reading are separate
 *   constraints with their own ε, victim sets, and stakeholder surfaces,
 *   linked here only through the kernel and network fields.
 *
 * KEY AGENTS:
 *   - animal_owners: primary beneficiary (moderate/mobile) — holds clear title and use rights
 *   - livestock_industry: primary institutional beneficiary (organized/arbitrage) — capitalizes animals as productive assets
 *   - biomedical_research_sector: institutional beneficiary (institutional/arbitrage) — uses animals as research property under procedural welfare review
 *   - courts_and_legislatures: agenda_setter (institutional/analytical) — maintains and adjudicates the property default
 *   - animal_rights_advocates: excluded voice (organized/constrained) — argues for standing the reading does not grant
 *   - companion_animals_livestock_research_subjects: non-agent entity listed for narrative completeness — the reading's own premise denies them party status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.06).
domain_priors:suppression_score(animal_status__property_reading, 0.15).
domain_priors:theater_ratio(animal_status__property_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animal-as-Property Legal Standing (Property Reading)").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '3081d563-57e6-422a-b531-0bcf94999887').
narrative_ontology:cs_kernel_codification('3081d563-57e6-422a-b531-0bcf94999887', distributed).
narrative_ontology:cs_authority_grounding('3081d563-57e6-422a-b531-0bcf94999887', practice).
narrative_ontology:cs_interpretation_layer_present('3081d563-57e6-422a-b531-0bcf94999887').
narrative_ontology:cs_reading_relation('3081d563-57e6-422a-b531-0bcf94999887', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('3081d563-57e6-422a-b531-0bcf94999887', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('3081d563-57e6-422a-b531-0bcf94999887', foundational, animals_lack_independent_legal_standing).
narrative_ontology:cs_axiom_status(animals_lack_independent_legal_standing, holdable).
narrative_ontology:cs_axiom_grounding('3081d563-57e6-422a-b531-0bcf94999887', animals_lack_independent_legal_standing, conventional).
narrative_ontology:cs_axiom('3081d563-57e6-422a-b531-0bcf94999887', secondary, ownership_rights_default_unrestricted_absent_statute).
narrative_ontology:cs_axiom_status(ownership_rights_default_unrestricted_absent_statute, holdable).
narrative_ontology:cs_axiom_grounding('3081d563-57e6-422a-b531-0bcf94999887', ownership_rights_default_unrestricted_absent_statute, conventional).
narrative_ontology:cs_reference_frame('3081d563-57e6-422a-b531-0bcf94999887', common_law_chattel_doctrine).
narrative_ontology:cs_drift_state('3081d563-57e6-422a-b531-0bcf94999887', contemporary_sentience_science_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3081d563-57e6-422a-b531-0bcf94999887', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, livestock_industry).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, biomedical_research_sector).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, pet_breeding_industry).
narrative_ontology:constraint_vindicates(animal_status__property_reading, property_based_ownership_doctrine).
narrative_ontology:constraint_vindicates(animal_status__property_reading, human_exclusive_moral_standing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold clear, transferable legal title over animals as chattel: can buy, sell, breed, confine, use for labor or companionship, and dispose of animals within the bounds of welfare statutes. The property framing gives certainty and enforceability to their claims against third parties (theft, injury to their animal, custody disputes) without requiring them to justify their use of the animal to the animal itself.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_owners, beneficiary,
    moderate, biographical, mobile, national).

% Operates confinement, breeding, and slaughter operations on the legal premise that animals are inventory and productive assets. This premise is what makes industrial-scale animal agriculture legally and financially tractable — animals are capitalized, insured, and depreciated as property, and welfare statutes set only a floor rather than requiring justification of the underlying use.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, livestock_industry, beneficiary,
    organized, generational, arbitrage, national).

% Uses animals as research subjects and property of the laboratory or institution, subject only to procedural welfare oversight (IACUC-style review) rather than any independent standing claim by the animal. This lets research proceed without treating the animal as a party whose interests could categorically bar the experiment.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, biomedical_research_sector, beneficiary,
    institutional, generational, arbitrage, national).

% Breeds and sells animals as commodities; ownership transfer, valuation, and contractual warranty of the animal function exactly as with any other tangible good, giving the industry legal predictability.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, pet_breeding_industry, beneficiary,
    moderate, biographical, mobile, national).

% Argue that treating sentient beings as objects incapable of holding any legal interest is a category error with material consequences (no standing to sue on the animal's behalf, no independent weighing of the animal's interest against the owner's). They participate in litigation and legislative advocacy but are not parties to the property relationship itself, which the courts and legislatures continue to structure around owner interests.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_rights_advocates, excluded,
    organized, generational, constrained, national).

% Maintain the property classification as the default legal status of animals, layering welfare statutes (anti-cruelty law, transport and slaughter regulation) on top without disturbing the underlying object status. They adjudicate disputes as human-to-human property conflicts (custody, damages, theft) and set welfare floors through ordinary regulatory processes rather than rights adjudication.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% The entities whose treatment the arrangement governs, but who under this reading hold no independent legal interest and cannot appear as a party. Listed for completeness of the narrative; the property reading does not treat them as an agent capable of being a stakeholder in the constraint-theoretic sense — this is the reading's own premise, not an empirical claim this story adjudicates.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, companion_animals_livestock_research_subjects, excluded,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(animal_status__property_reading, companion_animals_livestock_research_subjects).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, well-understood legal category (property) that lets ownership, transfer, breeding, use, and injury claims involving animals be resolved with the same doctrinal tools used for any other chattel, avoiding the need to construct a novel legal category of partial personhood for every transaction and dispute.
% TRANSFER_FUNCTION: Moves the burden of proof and the default entitlement toward the owner: an owner need not justify their use of an animal against any independent interest the animal is deemed to hold, and welfare statutes function as exceptions carved into ownership rather than baseline rights the owner must overcome. Disputes about animals are resolved as transfers of value or liability between humans.
% ABSENT_VOICES: Animal rights advocates argue for standing on the animal's behalf but are not parties to the ownership relationship and cannot bring suit in the animal's own interest under this reading; the animals themselves are, by the reading's own premise, incapable of holding a voice in the proceeding at all.
% DISAPPEARANCE_RATIONALE: If the property classification vanished overnight, livestock capitalization, insurance, breeding contracts, laboratory use authorization, and even routine veterinary consent structures would need to be rebuilt around some form of independent animal interest or guardianship — the entire economic and legal apparatus of animal use as currently practiced depends on animals being freely alienable objects.
% FOUNDING_PROBLEM: Historical legal systems needed a workable way to allocate control over animals used for labor, food, transport, and companionship among human claimants, and property law was the ready-made doctrinal toolkit for allocating control over any valuable, controllable, non-human thing.
% FOUNDING_PROBLEM_CORROBORATION: Livestock and research-sector representatives attest the founding problem (allocating control and enabling productive use) remains fully live and the property framework remains fit for purpose. Animal law scholars and welfare-statute drafters — outside the beneficiary set — attest that the scientific and social consensus on animal sentience has substantially outrun the object-status premise, making the doctrinal fit contested rather than settled; no independent corroboration exists from a neutral party who denies any interest is at stake, since that denial is the reading's own axiom rather than an external finding.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).
:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.06 at interval end) because, from this reading's own lights, there is no cognizable victim: animals are not a party capable of being extracted from, and disputes that do occur are ordinary human-to-human property or contract disputes (theft, custody, breach of sale warranty) with low intrinsic extraction. Suppression is low-moderate (0.15) reflecting only the ordinary enforcement of property and contract law, not any coercive apparatus aimed at animals (who are not, under this reading, suppressible parties) or at owners (whose use rights are largely unrestricted). Theater ratio is low and rises only slightly over the interval, tracking the modest growth of welfare-statute compliance paperwork layered onto an otherwise functional property regime. Accessibility collapse is moderate (0.35) — alternative legal framings (welfare-interest, rights-holder) are visible and actively argued in courts and legislatures, so alternatives have not collapsed the way they would for a true mountain. Resistance is low-moderate (0.2), coming from advocacy and litigation pressure rather than from the animals themselves, who structurally cannot resist within this framework.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter and beneficiary seats, the arrangement reads as a functional, low-friction Rope: a settled doctrinal category that lets ownership and use proceed with minimal transaction cost, welfare statutes operating as sensible add-on floors. From the excluded advocate seat, the same arrangement reads as a foreclosure of any interest-weighing at all — not extraction in the classic sense (there is no victim under this reading's own terms) but a structural silence that advocates read as itself the harm. The engine computes seat divergence from the structural data; this story records that divergence without resolving it, because resolving it would mean adopting a different reading (see omegas).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (animal_owners, livestock_industry, biomedical_research_sector, pet_breeding_industry) sit near the full-beneficiary end: they hold enforceable, unrestricted use rights subject only to statutory floors they had input into shaping. Courts and legislatures sit as institutional agenda-setters with analytical exit — they administer rather than extract or receive. Animal rights advocates are excluded rather than victimized under this reading's structure: they bear no direct cost from the property classification (they are third parties to it) but are denied standing to contest it on the animal's behalf, which is a structural exclusion rather than an extraction. No victim group is declared because the reading's defining premise is that no party capable of being extracted from exists — declaring animals as victims would be authoring the welfare or abolitionist reading's premise inside this story, which Rule 1 forbids.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating practical control over animals used for labor, food, and companionship — remains partially live (courts still need doctrinal tools to resolve ownership and use disputes) but is contested as a full account of the underlying moral situation, since the object-status premise has not kept pace with scientific consensus on animal sentience. This story does not resolve that contest; it records the mismatch (status=contested + verdict=world_rearranges) as the input the mandatrophy-detection consumer reads, without smuggling a verdict about which reading is correct into the metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_property_vs_siblings,
    'Is the property reading the structurally correct account of animal legal status, or does it foreclose morally relevant interests that the welfare or abolitionist readings would recognize?',
    'This is a genealogical and normative question, not an empirical one resolvable within this story: it depends on which theory of moral status (sentience-based, interest-based, capacity-based) is adopted as the framework''s premise. Comparative jurisdictional analysis (e.g., jurisdictions that have granted limited legal personhood or standing to animals) provides partial empirical evidence about the practical consequences of each reading but does not settle which reading is ''correct.''',
    'If the welfare or abolitionist premise is adopted instead, the beneficiary/victim structure of this exact arrangement inverts: the same ownership and use practices would be reclassified with animals in the victim set and extractiveness would rise substantially (the welfare_reading and abolitionist_reading sibling stories model this explicitly). This story''s near-zero ε is entirely an artifact of the property reading''s own premise and should never be cited as evidence against the sibling readings'' much higher ε values.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_property_vs_siblings, conceptual, 'Whether the property reading''s foreclosure of animal standing is itself defensible, versus an artifact of the reading chosen.').

omega_variable(
    welfare_statute_carveout_stability,
    'Are welfare statutes best understood as exceptions carved into an otherwise unrestricted property right (this reading''s framing), or as the leading edge of an emerging independent-interest doctrine that will eventually displace the property default?',
    'Track whether welfare statutes are being extended, narrowed, or reinterpreted by courts as establishing free-standing interests versus remaining strictly statutory and owner-permissive; a trend toward courts recognizing animal standing to sue (even via human guardians) would be evidence against the stability of the pure property framing.',
    'If welfare statutes are drift toward independent-interest doctrine, this reading''s own reference frame is eroding even on its own terms, which would be relevant to the cs_structure drift_state characterization for any future revision of this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statute_carveout_stability, empirical, 'Whether welfare-statute expansion is compatible with or corrosive to the pure property reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(anim_tr_t8, animal_status__property_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement(anim_tr_t16, animal_status__property_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement(anim_tr_t24, animal_status__property_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(anim_tr_t32, animal_status__property_reading, theater_ratio, 32, 0.095).
narrative_ontology:measurement(anim_tr_t40, animal_status__property_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(anim_be_t8, animal_status__property_reading, base_extractiveness, 8, 0.045).
narrative_ontology:measurement(anim_be_t16, animal_status__property_reading, base_extractiveness, 16, 0.05).
narrative_ontology:measurement(anim_be_t24, animal_status__property_reading, base_extractiveness, 24, 0.055).
narrative_ontology:measurement(anim_be_t32, animal_status__property_reading, base_extractiveness, 32, 0.058).
narrative_ontology:measurement(anim_be_t40, animal_status__property_reading, base_extractiveness, 40, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_status__property_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint, animal_status__welfare_reading, and animal_status__abolitionist_reading are three readings of a single contested kernel (animal_status): whether animals hold independent moral standing. Per the ε-invariance principle, each reading is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification rather than as one story with an observable parameter. This (property) reading authors near-zero ε (~0.06) because, by its own premise, no party capable of being extracted from exists in the ownership relationship. The welfare reading is expected to author a mid-range ε reflecting animals as a partially-weighed interest constrained by but not excluded from use. The abolitionist reading is expected to author a high ε with animals as the primary victim class of current instrumental-use practice. All three are linked via affects_constraints to preserve the family relationship for contamination and network analysis without conflating their distinct classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
