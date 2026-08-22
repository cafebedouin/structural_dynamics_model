% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__entangled_event_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: Statute of Anne (1710) as Entangled Conceptual-Institutional Founding Event
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is conventionally cited as the origin of
 *   modern copyright, but the label conflates two structurally distinct
 *   claims about what happened: that a new conceptual category of intangible
 *   authorial property came into being, and that existing institutional
 *   control over printed works was reallocated from the Stationers' Company
 *   to authors (and thence, in practice, to publishers by assignment). This
 *   story instantiates a third reading: that the statute is best understood
 *   as a single, indivisible historical event in which the conceptual and
 *   institutional dimensions occurred simultaneously and cannot be separated
 *   by any coherent analytical procedure — attempts to disentangle them (as
 *   the sibling readings do) are themselves interpretive choices imposed
 *   after the fact, not discoveries of a pre-existing separable structure.
 *   Under this reading, the beneficiary structure is irreducibly ambiguous:
 *   authors are named beneficiaries in the statutory text, but publishers are
 *   the practical beneficiaries who administer, litigate, and ultimately
 *   extend the regime through common-law copyright arguments culminating in
 *   Donaldson v Becket (1774). The chief victim of the entanglement is not a
 *   class of persons directly but the possibility of a clean doctrinal
 *   foundation — a cost borne diffusely by everyone who has since tried to
 *   answer 'is copyright a natural right or a statutory grant?' without a
 *   stable answer.
 *
 * KEY AGENTS:
 *   - authors_nominal: nominal statutory beneficiary, practically without exit from assignment pressure
 *   - publishers_practical: practical administrator and long-run beneficiary through assignment and litigation
 *   - conceptual_clarity_of_property_form: non-agent casualty of the entanglement itself
 *   - public_domain_users: diffuse payer, excluded from the interpretive contest
 *   - rival_provincial_printers: excluded party with a stake in disambiguation
 *   - legal_historians: analytical observers whose persistent disagreement is evidence for this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.58).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.52).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne (1710) as Entangled Conceptual-Institutional Founding Event").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, 'e23eeec6-e98b-4304-8538-27df86522ddb').
narrative_ontology:cs_kernel_codification('e23eeec6-e98b-4304-8538-27df86522ddb', fixed_text).
narrative_ontology:cs_authority_grounding('e23eeec6-e98b-4304-8538-27df86522ddb', lineage).
narrative_ontology:cs_interpretation_layer_present('e23eeec6-e98b-4304-8538-27df86522ddb').
narrative_ontology:cs_reading_relation('e23eeec6-e98b-4304-8538-27df86522ddb', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e23eeec6-e98b-4304-8538-27df86522ddb', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_axiom('e23eeec6-e98b-4304-8538-27df86522ddb', foundational, conceptual_and_institutional_change_are_jointly_constituted).
narrative_ontology:cs_axiom_status(conceptual_and_institutional_change_are_jointly_constituted, holdable).
narrative_ontology:cs_axiom_grounding('e23eeec6-e98b-4304-8538-27df86522ddb', conceptual_and_institutional_change_are_jointly_constituted, conventional).
narrative_ontology:cs_axiom('e23eeec6-e98b-4304-8538-27df86522ddb', secondary, beneficiary_identity_is_irreducibly_ambiguous_between_authors_and_publishers).
narrative_ontology:cs_axiom_status(beneficiary_identity_is_irreducibly_ambiguous_between_authors_and_publishers, holdable).
narrative_ontology:cs_axiom_grounding('e23eeec6-e98b-4304-8538-27df86522ddb', beneficiary_identity_is_irreducibly_ambiguous_between_authors_and_publishers, empirically_contingent).
narrative_ontology:cs_reference_frame('e23eeec6-e98b-4304-8538-27df86522ddb', single_indivisible_founding_act).
narrative_ontology:cs_drift_state('e23eeec6-e98b-4304-8538-27df86522ddb', post_donaldson_v_becket_1774, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e23eeec6-e98b-4304-8538-27df86522ddb', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity_of_property_form).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, public_domain_users).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__entangled_event_reading, copyright_as_statutory_grant_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Named in the statute's preamble as the intended rights-holder and encouragement target. Gains a formal, statutorily-created entitlement — a 14-year term, renewable once — where none existed as a distinct legal category before. In practice, most authors immediately assign this entitlement to a bookseller for a lump sum or ongoing arrangement, because the statute created the right without creating an infrastructure (distribution, printing capital, litigation capacity) that individual authors could exploit on their own.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal, beneficiary,
    powerless, biographical, constrained, national).

% Former Stationers' Company members who lobbied for the statute after losing the perpetual common-law-adjacent licensing monopoly under the Printing Act's lapse. They administer the new regime in practice: they acquire authors' statutory rights by assignment, they litigate infringement, and they lobby subsequent Parliaments and courts (notably in Millar v Taylor and Donaldson v Becket) to reinterpret the statute's term limits as compatible with a perpetual common-law copyright. They set the terms under which the 'author's right' is actually exercised.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical, agenda_setter).

% Not an actor but a casualty of the entangled event: because the statute's conceptual innovation (a new intangible property in expression) and its institutional reallocation (moving control from guild to author-then-assignee) happened in the same textual act, subsequent courts, treatise writers, and historians spend a century and a half unable to say cleanly whether copyright is a natural right recognized by statute, a statutory creation ex nihilo, or a reallocated guild privilege. The entanglement itself is what is paid — the doctrine never achieves a clean theoretical foundation.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity_of_property_form, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity_of_property_form).

% Readers, printers of out-of-term works, and later authors who build on existing texts. They bear the cost of whichever the ambiguity resolves toward at a given moment: when courts lean toward the 'natural right' reading (as in the pre-Donaldson period), the term limits are treated as a floor rather than a ceiling and works stay locked up longer than the statute's face text promises. They cannot participate in the interpretive contest — they are not represented in the litigation between publishers and rival publishers that settles the question in 1774.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, public_domain_users, payer,
    powerless, generational, trapped, national).

% Printers outside the London trade who would benefit from an early, clean statutory-limit reading (fixed terms, no perpetual common-law overlay) because it would open reprint markets sooner. Their interest in disambiguating the statute's dual nature toward the institutional-reallocation reading is real but they have no seat in the House of Lords litigation that settles the question — the case (Donaldson v Becket) is fought between London-centered commercial interests.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, rival_provincial_printers, excluded,
    moderate, biographical, constrained, regional).

% Examine the statute's text, parliamentary debates, and subsequent case law to determine whether the conceptual and institutional dimensions can be analytically separated after the fact. Their scholarly disagreement (Rose, Deazley, Patterson, and others reach different conclusions) is itself evidence for the entangled-event reading — if the dimensions were cleanly separable, the historical debate would likely have converged.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__entangled_event_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The statute solves a genuine coordination failure left by the 1695 lapse of the Licensing Act: without any registration or term system, there was no stable basis for anyone (author, publisher, or public) to know when a work's exclusive commercial control began or ended. The statute supplies a single registration-and-term mechanism that all parties can consult.
% TRANSFER_FUNCTION: The statute simultaneously creates a new form of intangible entitlement (the conceptual dimension — an author's statutory interest in a printed work, previously nonexistent as a distinct legal category) and moves practical control over that entitlement from the Stationers' Company's perpetual entry-book monopoly to a term-limited, nominally author-held, practically publisher-assigned right (the institutional dimension). Because both happen in the same eighteen sections of the same act, no coherent transfer_function can be stated without invoking both: the thing being moved and the entity now capable of holding it are created at once.
% ABSENT_VOICES: Public domain users and provincial reprint printers would object that the ambiguity between 'new right' and 'reallocated right' was later exploited by London booksellers to argue for a perpetual common-law copyright underneath the statutory term — extending effective control well past 14/28 years. They had no standing in the 1774 Donaldson v Becket litigation that finally resolved the question against perpetuity, and no voice in the 1710 Parliament that produced the ambiguous text in the first place.
% DISAPPEARANCE_RATIONALE: If the statute (and the entangled reading of it) were subtracted from legal history, there is no single antecedent event to fall back to: the Stationers' Company monopoly had already lapsed and was not reinstated, so removing the statute does not restore a prior stable order — it removes the only account, however tangled, of how a purely guild-based licensing privilege became a rights-holder-based statutory entitlement. Copyright doctrine, term structure, and the very question of whether authors or publishers are the 'real' rights-holders would have to be rebuilt from different materials.
% FOUNDING_PROBLEM: Parliament needed to solve two problems that arrived together in 1710: (1) the collapse of press regulation and licensing enforcement after 1695, which threatened the book trade's capital investment model, and (2) the absence of any legal category recognizing an author's interest in their own composition, which the trade's lobbying (via figures like Daniel Defoe) framed as a moral and economic argument for a statutory answer.
% FOUNDING_PROBLEM_CORROBORATION: The Stationers' Company's own petitions to Parliament (preserved in the Journals of the House of Commons, 1706-1710) attest that the immediate founding problem was the trade's loss of enforcement machinery, not authorial recognition as such — corroboration from outside the beneficiary set. Modern legal historians (Ronan Deazley, Mark Rose) independently attest, from primary-source review rather than trade advocacy, that the statute's authorial framing was substantially a rhetorical vehicle for restoring publisher enforcement capacity, which supports reading the founding problem as still partly live (enforcement) and partly resolved-then-recharacterized (authorial recognition).
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high 0.58 because, under the entangled reading, the coordination function (a workable term-and-registration system) is real but the same act simultaneously installs the interpretive uncertainty that publishers later exploit to argue for perpetual control — the extraction is inseparable from the coordination, which is exactly the tangled-rope signature. Suppression (0.52) reflects that alternative, disentangled framings were foreclosed in practice not by explicit prohibition but by the fact that the only forum for resolving the ambiguity (chancery and King's Bench litigation, then the House of Lords in 1774) was dominated by the same commercial publishing interests who benefited from prolonged ambiguity. Theater ratio rises across the interval (0.20 to a peak of 0.38 before 1774) as publishers increasingly invoke moral and natural-rights rhetoric about authorial genius that the statute's actual text does not clearly support, using the entanglement's ambiguity as cover; it drops back to 0.32 after Donaldson v Becket forces at least partial doctrinal resolution against perpetuity.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors are named beneficiaries by the statutory text but their exit options are constrained by the trade's capital structure — publishing requires printing capital and distribution networks authors typically lack, so the 'right' functions as an asset to be sold rather than exploited directly, placing authors closer to symmetric-to-target than pure beneficiary despite the nominal framing. Publishers hold organized power and arbitrage-grade exit (they can lobby for reinterpretation, forum-shop across courts, and hold the actual administrative machinery), which is why they are the practical beneficiary despite not being named in the statute's preamble. Conceptual clarity is authored as a non-agent payer to represent that the cost of entanglement is borne by the coherence of the legal category itself, not redistributed to any single human actor — this is deliberately unusual and is why it carries agent: false.
 *
 * MANDATROPHY ANALYSIS:
 *   The entangled-event reading resists the mandatrophy trap in both directions: it does not let the coordination story (a genuine registration-and-term mechanism was needed and was supplied) launder the extraction (publishers used the statute's ambiguity to pursue perpetual control for over sixty years), nor does it let the extraction story (publishers captured the regime) erase that the statute did supply a real, previously absent, workable public mechanism for term limits that public domain users eventually benefited from once Donaldson v Becket resolved the ambiguity in 1774. Classifying this as tangled_rope rather than pure snare or pure rope preserves both halves as simultaneously true and structurally fused, which is the reading's entire content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    separability_of_conceptual_and_institutional_dimensions,
    'Can the conceptual innovation (a new form of intangible authorial property) and the institutional reallocation (control moving from guild to author-then-publisher) be given independent historical or doctrinal accounts, or is the entangled_event_reading correct that no coherent separation is possible?',
    'Close textual and archival analysis of the statute''s drafting history and contemporaneous parliamentary debate: if drafters and contemporaries treated the conceptual category and the institutional transfer as decisions made at different times or for different reasons (even within the same bill), that would support separability and favor one of the sibling readings; if the sources show the two decided as a single unresolved bundle, that supports this reading.',
    'If separable, this constraint should be retired or merged into whichever sibling reading captures the analytically prior dimension, and this story''s ambiguous beneficiary/victim structure would resolve into the cleaner structures of the sibling stories. If genuinely inseparable, the entangled reading is the historically accurate one and the sibling readings are useful analytic fictions rather than competing historical claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_conceptual_and_institutional_dimensions, conceptual, 'Whether the statute''s conceptual and institutional dimensions are analytically separable or genuinely fused.').

omega_variable(
    beneficiary_ambiguity_resolution,
    'Is the ambiguity between authors (nominal beneficiary) and publishers (practical beneficiary) itself evidence for the entangled reading, or is it simply the ordinary and separable phenomenon of a formal right being commercially assigned — which any of the three readings could accommodate?',
    'Compare against contemporaneous statutory grants of formal rights that were NOT bundled with a simultaneous conceptual innovation (e.g., other 18th-century trade privilege statutes) to see whether assignment-driven beneficiary ambiguity arises independently of conceptual novelty.',
    'If beneficiary ambiguity is a generic feature of assignable rights unrelated to conceptual novelty, it weakens this reading''s distinctive claim and strengthens the institutional_reallocation_reading (which treats the mechanism as ordinary reallocation, not a fused act). If the ambiguity is specifically produced by the novelty of the right having no prior holder-type to model assignment on, it strengthens the entangled reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_ambiguity_resolution, empirical, 'Whether beneficiary ambiguity is diagnostic of entanglement or an ordinary feature of assignable rights.').

omega_variable(
    committer_framing_underdetermination,
    'Is the choice to author a third, entangled reading (rather than treating the kernel as fully resolved by the conceptual_emergence and institutional_reallocation readings jointly) itself a defensible structural claim, or does it risk becoming an unfalsifiable synthesis that absorbs any evidence either sibling reading could produce?',
    'Specify, in advance, what historical evidence would count against the entangled reading specifically — e.g., a clear documentary record showing Parliament debated and settled the conceptual question (what kind of thing is being created) independently of and prior to the institutional question (who holds it) would falsify entanglement in favor of sequential separability.',
    'Without a stated falsification condition, the entangled reading risks being unfalsifiable by construction (any evidence for either sibling reading could be redescribed as ''part of the same entangled act''). Establishing the falsification condition disciplines the reading and clarifies what would change the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_underdetermination, conceptual, 'Whether the entangled-event reading is falsifiable or an unfalsifiable synthesis of its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1710, 0.2).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1720, 0.24).
narrative_ontology:measurement(stat_tr_t1731, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1731, 0.28).
narrative_ontology:measurement(stat_tr_t1745, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1745, 0.31).
narrative_ontology:measurement(stat_tr_t1760, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1760, 0.35).
narrative_ontology:measurement(stat_tr_t1769, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1769, 0.38).
narrative_ontology:measurement(stat_tr_t1774, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1774, 0.32).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1710, 0.42).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1720, 0.46).
narrative_ontology:measurement(stat_be_t1731, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1731, 0.5).
narrative_ontology:measurement(stat_be_t1745, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1745, 0.55).
narrative_ontology:measurement(stat_be_t1760, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1760, 0.58).
narrative_ontology:measurement(stat_be_t1769, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1769, 0.62).
narrative_ontology:measurement(stat_be_t1774, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1774, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statute_of_anne_ip_foundation__entangled_event_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__entangled_event_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, institutional_reallocation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the entangled_event_reading in a three-member family reading the statute_of_anne_ip_foundation kernel. conceptual_emergence_reading claims the statute's primary significance is the creation of a new conceptual category (copyright as a limited regulatory tool for learning); institutional_reallocation_reading claims its primary significance is control moving from the Stationers' Company to authors, with the conceptual form treated as pre-existing or incidental. This story claims neither can be authored as historically prior to the other — the two dimensions are fused in the same textual and political act — and authors its own ε (0.58, tangled_rope) reflecting the fusion of a genuine coordination function with the extraction that ambiguity enabled. All three stories share the kernel but differ in beneficiary/victim structure and in claimed_type; none averages or defers to the others per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
