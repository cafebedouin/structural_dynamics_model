% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: Statute of Anne (1710) — Entangled Event Reading
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is the originating event of modern copyright.
 *   This reading holds that the statute's conceptual innovation (copyright as
 *   a limited regulatory grant for learning) and its institutional innovation
 *   (vesting the initial right in the author, not the printer) are
 *   inseparable dimensions of a single legislative act. They were not debated
 *   as alternatives; they arrived fused in the bill's text and the political
 *   bargain that passed it. The entanglement means later IP doctrine inherits
 *   a fused object: the property form and the regulatory justification cannot
 *   be cleanly separated because they were never separate in the originating
 *   event. Beneficiaries are ambiguous — authors are named in the statute but
 *   publishers captured the practical value; victims include conceptual
 *   clarity (the analytical distinction lost) and the public domain (slower
 *   expansion than the term structure suggested).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.58).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.62).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne (1710) — Entangled Event Reading").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, '59700bab-7b84-4741-b2fb-5b1d7c9622ba').
narrative_ontology:cs_kernel_codification('59700bab-7b84-4741-b2fb-5b1d7c9622ba', formalized).
narrative_ontology:cs_authority_grounding('59700bab-7b84-4741-b2fb-5b1d7c9622ba', lineage).
narrative_ontology:cs_interpretation_layer_present('59700bab-7b84-4741-b2fb-5b1d7c9622ba').
narrative_ontology:cs_reading_relation('59700bab-7b84-4741-b2fb-5b1d7c9622ba', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('59700bab-7b84-4741-b2fb-5b1d7c9622ba', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_axiom('59700bab-7b84-4741-b2fb-5b1d7c9622ba', foundational, conceptual_institutional_fusion_at_origin).
narrative_ontology:cs_axiom_status(conceptual_institutional_fusion_at_origin, holdable).
narrative_ontology:cs_axiom_grounding('59700bab-7b84-4741-b2fb-5b1d7c9622ba', conceptual_institutional_fusion_at_origin, conventional).
narrative_ontology:cs_axiom('59700bab-7b84-4741-b2fb-5b1d7c9622ba', secondary, author_as_first_owner_is_regulatory_fiction).
narrative_ontology:cs_axiom_status(author_as_first_owner_is_regulatory_fiction, holdable).
narrative_ontology:cs_axiom_grounding('59700bab-7b84-4741-b2fb-5b1d7c9622ba', author_as_first_owner_is_regulatory_fiction, empirically_contingent).
narrative_ontology:cs_reference_frame('59700bab-7b84-4741-b2fb-5b1d7c9622ba', parliamentary_bargain_1710).
narrative_ontology:cs_drift_state('59700bab-7b84-4741-b2fb-5b1d7c9622ba', post_1842_act, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59700bab-7b84-4741-b2fb-5b1d7c9622ba', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, public_domain_expansion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Named as primary beneficiaries of the statute (14-year term, renewable once). In practice, their bargaining position against publishers remained weak; they often assigned rights outright. The statute gave them a legal foothold but did not equalize the power asymmetry in the book trade. Exit from publisher dependence was constrained by the economics of printing and distribution.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal, payer).

% The Stationers' Company lost its perpetual monopoly but captured the new statutory copyright as tradeable commercial rights. They drafted the bill, lobbied for it, and structured the book trade around the new term-limited rights they could buy from authors. They retained control of printing, distribution, and the register of entries. Their exit options were strong — they could adapt the new framework to their existing commercial infrastructure.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical, agenda_setter).

% The statute fused two distinct moves — creating a new conceptual category (copyright as limited regulatory grant) and instantiating it as a property right held by authors — into a single legislative act. This entanglement made it structurally difficult for later jurists and theorists to separate the regulatory justification from the property form. The victim is not a person but the analytical distinction that would have allowed cleaner evolution of IP doctrine.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity).

% The 28-year maximum term (14+14) was a concession to learning, but the statute's property frame and registration requirements created a default of enclosure. Works not registered or not renewed fell into the public domain, but the administrative burden and publisher control of registration meant the public domain grew slower than the statutory term structure suggested. The conceptual/institutional entanglement obscured the public domain's structural role.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, public_domain_expansion, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__entangled_event_reading, public_domain_expansion).

% Lost its perpetual royal charter monopoly over printing but negotiated the statutory replacement. They paid the cost of surrendering perpetual control but gained a state-enforced, term-limited copyright they could trade. Their institutional identity shifted from monopoly guild to rights-administering body. Exit from the old model was forced; adaptation to the new model was managed from within.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company, payer).

% Enacted the statute as a legislative bargain: break the Stationers' perpetual monopoly, create a limited term for 'the encouragement of learning,' vest initial rights in authors. The entanglement of conceptual innovation (limited regulatory grant) and institutional reallocation (author as first owner) was not debated as a separable choice — it was the bill as drafted and passed.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, parliament_1710, agenda_setter,
    institutional, immediate, arbitrage, national).

% Inherit a doctrinal object (statutory copyright) whose conceptual justification and institutional form arrived fused. The entanglement structures centuries of debate: is copyright a natural property right of authors (institutional form) or a utilitarian regulatory tool (conceptual justification)? The reading that they are inseparable in the originating event shapes how later theorists frame the question.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, legal_theorists_post_1710, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Created a workable institutional compromise: replaced the Stationers' perpetual monopoly with a term-limited, state-enforced right that could be traded, giving authors a legal foothold while preserving the book trade's commercial infrastructure. Solved the coordination problem of how to regulate printing after the Licensing Act lapsed (1695) without collapsing the trade or leaving authors rightless.
% TRANSFER_FUNCTION: Transferred the legal title to control reproduction from the Stationers' Company (perpetual, guild-based) to authors (14+14 years, statutory), who in practice transferred it to publishers via assignment. The statute moved the locus of right-holding and the enforcement mechanism simultaneously.
% ABSENT_VOICES: The reading public / learners — the statute's preamble names 'the encouragement of learning' as the purpose, but no representative of readers, students, or competing printers outside the Stationers' Company was heard. The public domain's expansion was a structural byproduct, not a represented interest.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne vanished overnight, the entire genealogy of modern copyright — the author-as-first-owner principle, the term-limited regulatory grant, the registration/formalities tradition, the public domain as statutory residual — would lack its originating event. Common law copyright claims (perpetual, natural right) would reassert dominance in English courts; the book trade would revert to guild regulation or contractual chaos. The conceptual/institutional entanglement is the hinge on which the subsequent 300 years of IP law turns.
% FOUNDING_PROBLEM: The Licensing Act of 1662 (renewed periodically) had lapsed in 1695, leaving printing unregulated. The Stationers' Company claimed a perpetual common law copyright in registered works; independent printers and the book trade's commercial logic pressed for open competition. Authors had no independent legal standing. The founding problem was: how to regulate the book trade after state licensing ended, without restoring the Stationers' monopoly or abandoning authors entirely.
% FOUNDING_PROBLEM_CORROBORATION: The Licensing Act's lapse and the Stationers' monopoly claim are historically documented (Parliamentary records, Stationers' Court records, contemporary pamphlets). The 'encouragement of learning' preamble is the statute's own text. That the founding problem is dead — the regulatory crisis of 1695–1710 is resolved — is attested by the statute's own operation for 200+ years and its replacement by the 1842 Copyright Act. No living participant treats the 1695 vacuum as a current problem.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.58 at interval end) reflects the statute's property frame enabling publisher capture of author rights through assignment, while the limited term and registration requirements provided some public domain protection. Suppression (0.62) captures the active enforcement needed: registration formalities, penalties for unregistered printing, and the Stationers' Company's continued control of the register. Theater ratio (0.28) reflects that the 'encouragement of learning' justification was genuine but increasingly performative as publisher control solidified. The measurement series uses a shared grid (1710, 1735, 1760, 1785, 1810, 1842) covering the statute's active life until the 1842 Act.
 *
 * PERSPECTIVAL GAP:
 *   From the publishers' seat, the statute is a successful coordination mechanism (rope-like) that replaced a broken monopoly with a tradeable rights system. From the authors_nominal seat, it is a tangled rope — genuine coordination (they got rights) fused with extraction (publishers captured the value). From the conceptual_clarity seat (analytical), it is a snare — the entanglement suppresses the analytical separation that would allow cleaner doctrinal evolution. The engine computes this seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers_practical are the structural beneficiaries (d near beneficiary end): they captured the tradeable rights, controlled registration, and adapted their commercial infrastructure. Authors_nominal sit near symmetric (d ~0.5): statutory beneficiaries but practically constrained exit, often assigning rights away. Conceptual_clarity and public_domain_expansion are victims (d near target end): the entanglement structurally extracts analytical distinction and public domain growth. Parliament_1710 and Stationers_Company are agenda_setters with arbitrage/constrained exit. Legal_theorists_post_1710 are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-1695 regulatory vacuum) is dead — the statute solved it. The arrangement persists (via successor statutes) because the entangled conceptual/institutional form proved adaptable to new technologies and commercial pressures, not because the original problem remains. The mandate has not atrophied into pure inertia; it has been actively rewritten. But the entanglement means each rewrite inherits the fused structure, making pure 'regulatory tool' or pure 'property right' readings structurally difficult to instantiate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_institutional_separability,
    'Could the Statute of Anne have instantiated the limited regulatory grant (conceptual) without vesting initial rights in authors (institutional), or vice versa?',
    'Counterfactual historical analysis: examine the drafting history, parliamentary debates, and the Stationers'' petition to see whether the two dimensions were ever proposed as separable options. If the bill''s sponsors treated them as a package, the entanglement is structural; if alternatives were debated, the fusion was a contingent political choice.',
    'If separable alternatives existed and were rejected, the entanglement is a chosen fusion (supporting the reading that the fused form carries extractive load). If no alternatives were thinkable at the time, the entanglement is a historical necessity (supporting the reading that later doctrinal confusion is an artifact of origin, not design).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_institutional_separability, conceptual, 'Whether the conceptual/institutional entanglement is structural necessity or contingent political fusion.').

omega_variable(
    author_beneficiary_reality,
    'Did authors_nominal actually benefit from the statute in net terms, or was the author-as-first-owner provision a legislative fiction that transferred value to publishers_practical?',
    'Economic history of the 18th-century book trade: contract terms, assignment rates, author earnings vs. publisher profits, litigation patterns (e.g., Donaldson v Beckett 1774). Compare author outcomes under the statute to the pre-1710 regime.',
    'If authors_nominal were net extractees (assigning rights for less than their value), the statute''s beneficiary structure is deceptive — publishers_practical are the true beneficiaries, making the constraint more snare-like. If authors gained meaningful leverage, the tangled_rope classification (genuine coordination + asymmetric extraction) holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(author_beneficiary_reality, empirical, 'Whether the statute''s nominal beneficiaries (authors) were net beneficiaries or net payers in practice.').

omega_variable(
    public_domain_measurement,
    'How much did the public domain actually expand under the Statute of Anne''s registration and term limits, relative to a counterfactual of perpetual common law copyright?',
    'Bibliometric analysis of Stationers'' Register entries, renewal rates, and survival of works. Compare the statutory public domain flow to the pre-1710 common law claim and to the post-1842 regime.',
    'If the public domain expanded substantially, the statute''s coordination function (limited term for learning) has empirical support. If expansion was minimal (low registration, low renewal, publisher control of the register), the ''encouragement of learning'' justification is increasingly theatrical — higher theater_ratio, more snare-like.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_domain_measurement, empirical, 'Empirical magnitude of the statute''s public domain effect vs. its stated purpose.').

omega_variable(
    kernel_reading_boundary,
    'Is the entangled_event_reading a distinct structural claim about the statute, or a meta-claim about the relationship between the conceptual_emergence_reading and institutional_reallocation_reading?',
    'Test whether the entangled reading makes predictions that differ from both siblings — e.g., about doctrinal evolution, reform trajectories, or comparative IP history. If it only says ''both are partially true,'' it may be a synthesis rather than a distinct constraint.',
    'If the entangled reading is a synthesis, it should not be a separate constraint story but a commentary on the sibling pair. If it makes distinct structural claims (e.g., that the fusion itself generates extraction), it warrants its own story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether this reading is a distinct constraint or a synthesis of the sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 1710, 1842).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1710, 0.15).
narrative_ontology:measurement(stat_tr_t1735, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1735, 0.18).
narrative_ontology:measurement(stat_tr_t1760, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1760, 0.22).
narrative_ontology:measurement(stat_tr_t1785, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1785, 0.25).
narrative_ontology:measurement(stat_tr_t1810, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1810, 0.27).
narrative_ontology:measurement(stat_tr_t1842, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1842, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1710, 0.45).
narrative_ontology:measurement(stat_be_t1735, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1735, 0.5).
narrative_ontology:measurement(stat_be_t1760, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1760, 0.52).
narrative_ontology:measurement(stat_be_t1785, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1785, 0.55).
narrative_ontology:measurement(stat_be_t1810, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1810, 0.57).
narrative_ontology:measurement(stat_be_t1842, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1842, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1710, 0.4).
narrative_ontology:measurement(stat_su_t1735, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1735, 0.48).
narrative_ontology:measurement(stat_su_t1760, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1760, 0.52).
narrative_ontology:measurement(stat_su_t1785, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1785, 0.56).
narrative_ontology:measurement(stat_su_t1810, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1810, 0.59).
narrative_ontology:measurement(stat_su_t1842, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1842, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__entangled_event_reading, 0.18).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, copyright_term_extension_1842).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, international_copyright_conventions_1886).

% DUAL FORMULATION NOTE:
% This reading is one of three in the statute_of_anne_ip_foundation constraint family. The conceptual_emergence_reading treats the statute as primarily creating a new conceptual category (limited regulatory grant). The institutional_reallocation_reading treats it as primarily moving existing rights from Stationers to authors. This entangled_event_reading treats the fusion as the structural fact. All three share the same historical referent (the 1710 statute) but author different ε values and different beneficiary/victim structures, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__entangled_event_reading, moderate, 0.45).
constraint_indexing:directionality_override(statute_of_anne_ip_foundation__entangled_event_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
