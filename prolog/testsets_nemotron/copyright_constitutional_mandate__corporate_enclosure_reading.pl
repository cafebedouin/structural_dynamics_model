% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright Constitutional Mandate — Corporate Enclosure Reading
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint story captures the corporate enclosure reading of the
 *   U.S. Constitution's Copyright Clause (Article I, Section 8, Clause 8: 'To
 *   promote the Progress of Science and useful Arts, by securing for limited
 *   Times to Authors and Inventors the exclusive Right to their respective
 *   Writings and Discoveries'). The corporate enclosure reading treats
 *   copyright as a natural property right deserving maximal protection,
 *   interprets 'limited Times' as permitting indefinite term extensions so
 *   long as each extension falls short of explicit perpetuity, and reads the
 *   'promote the Progress' preamble as hortatory rather than limiting. This
 *   reading has driven successive term extensions (1976 Act: life+50; 1998
 *   CTEA: life+70), criminalized circumvention of technological protection
 *   measures (DMCA §1201), and narrowed fair use through litigation pressure
 *   and contractual override. The constraint operates as a tangled rope: it
 *   retains a genuine coordination function (providing ex ante incentive for
 *   creative investment) while layering substantial asymmetric extraction
 *   onto that function (capturing value that would otherwise flow to the
 *   public domain, follow-on creators, and cultural commons). Beneficiaries
 *   are concentrated corporate rights holders (Disney, RIAA/MPAA member
 *   firms, collecting societies) who capture the rental stream; victims are
 *   derivative creators, educators, archivists, and independent artists whose
 *   creative and scholarly activities are constrained by expanded scope,
 *   extended term, and anti-circumvention rules. The reading coexists with
 *   two sibling readings of the same kernel: the public scaffold reading
 *   (copyright as temporary monopoly for public enrichment) and the judicial
 *   ambiguity reading (term length as legislative discretion subject to
 *   rational-basis deference).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.78).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.72).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright Constitutional Mandate — Corporate Enclosure Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, 'be7ee1cc-39b3-41c7-9d0d-3535de968ee4').
narrative_ontology:cs_kernel_codification('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', fixed_text).
narrative_ontology:cs_authority_grounding('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', extraction).
narrative_ontology:cs_interpretation_layer_present('be7ee1cc-39b3-41c7-9d0d-3535de968ee4').
narrative_ontology:cs_reading_relation('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', foundational, copyright_is_natural_property_right).
narrative_ontology:cs_axiom_status(copyright_is_natural_property_right, holdable).
narrative_ontology:cs_axiom_grounding('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', copyright_is_natural_property_right, deontological).
narrative_ontology:cs_axiom('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', foundational, limited_times_permits_indefinite_extension_short_of_perpetuity).
narrative_ontology:cs_axiom_status(limited_times_permits_indefinite_extension_short_of_perpetuity, holdable).
narrative_ontology:cs_axiom_grounding('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', limited_times_permits_indefinite_extension_short_of_perpetuity, conventional).
narrative_ontology:cs_reference_frame('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', founding_utilitarian_bargain).
narrative_ontology:cs_drift_state('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', post_ctea_eldred_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('be7ee1cc-39b3-41c7-9d0d-3535de968ee4', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_rights_holders).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, entertainment_lobby_coalition).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, collecting_societies).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators_and_researchers).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists_and_libraries).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, independent_artists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, independent_artists).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_as_natural_property_right).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, limited_times_permits_indefinite_extension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major entertainment and media corporations (Disney, Warner Bros, Universal, Sony, major publishers) that hold large copyright portfolios. They benefit from term extensions applied retroactively to existing works, from anti-circumvention rules that prevent format-shifting and interoperability, and from collecting society distributions. They write the legislative language through lobby coalitions (RIAA, MPAA, AAP) and litigate to expand scope. Their exit is arbitrage-grade: they can shift enforcement across jurisdictions, use trade agreements to harmonize upward, and capture regulatory agencies.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).

% The organized lobby apparatus (RIAA, MPAA, AAP, Copyright Alliance, Chamber of Commerce IP committees) that drafts legislation, coordinates litigation strategy, manages trade agreement negotiations (TRIPS, ACTA, USMCA IP chapters), and runs public messaging campaigns. They set the legislative agenda and define the enforcement priorities. They are not merely beneficiaries — they administer the constraint's expansion. Their exit is arbitrage-grade: they operate across jurisdictions and venues.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, entertainment_lobby_coalition, agenda_setter,
    institutional, generational, arbitrage, global).

% ASCAP, BMI, SESAC, SoundExchange, Harry Fox Agency, and foreign counterparts. They collect and distribute royalties, taking administrative cuts. They benefit from expanded scope (new rights to collect on) and extended terms (longer collection periods). They also set enforcement priorities through litigation and lobbying. Their exit is constrained: they depend on the statutory framework for their mandate and face competitive pressure from direct licensing.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, collecting_societies, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, collecting_societies, agenda_setter).

% Artists, writers, musicians, filmmakers, and coders whose work builds on existing culture: remix artists, fan fiction writers, documentary filmmakers needing archival footage, software developers needing API compatibility. They face clearance costs, licensing fees, litigation risk, and DMCA §1201 barriers to fair use. Their exit is identity-locked: their creative practice is constituted by engagement with the cultural corpus; leaving the constraint means abandoning their artistic vocabulary and community. They cannot 'choose not to create' without ceasing to be who they are as artists.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, identity_locked, national).

% Teachers, professors, students, and academic researchers who need to copy, adapt, translate, and distribute copyrighted works for pedagogy and scholarship. They face licensing fees, permission delays, fair use uncertainty, and DMCA barriers to text-and-data mining. Their exit is constrained: fair use provides some shelter but is fact-intensive and litigation-chilled; educational exceptions are narrow and jurisdiction-specific. They cannot easily substitute public domain materials for current scholarship.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators_and_researchers, payer,
    moderate, biographical, constrained, national).

% Libraries, archives, museums, and digital preservation initiatives (Internet Archive, HathiTrust, national libraries) that preserve cultural heritage. They face orphan works deadlock (cannot locate rights holders to license), format-shifting barriers (DMCA §1201 prevents circumventing DRM to preserve), and term lengths that exceed the commercial life of most works by decades. Their exit is trapped: they have a legal and ethical mandate to preserve; they cannot 'choose not to preserve' without violating their mission. The constraint actively prevents them from fulfilling their function.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists_and_libraries, payer,
    moderate, generational, trapped, national).

% Individual creators without corporate backing who both need copyright protection for their own work and need access to others' work as raw material. They benefit from the constraint's coordination function (protection against wholesale copying) but pay the extraction costs (clearance for samples, quotes, references; inability to use orphan works; DMCA barriers). Their exit is constrained: they need some copyright protection but the maximalist regime costs them more than it protects. They are structurally ambivalent — the constraint both helps and harms them, but the net flow is negative for those working in reference-heavy or collaborative forms.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, independent_artists, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, independent_artists, beneficiary).

% EFF, Public Knowledge, Authors Alliance, Creative Commons, library associations, academic clinics, and some law professors. They litigate, lobby, and publish analysis opposing term extensions, DMCA overreach, and fair use narrowing. They do not collect rents from the constraint nor bear its direct costs — they analyze its structural operation. Their seat is analytical: they see the full beneficiary/victim structure and the coordination/extraction hybrid. Their exit is analytical: they can leave the field but the constraint persists regardless.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_interest_advocates, observer,
    organized, generational, analytical, global).

% Article III judges who adjudicate copyright cases, particularly fair use, term extension challenges, and DMCA §1201 exemptions. The judicial_ambiguity reading (rational-basis deference) is the dominant judicial posture since Eldred v. Ashcroft (2003). They are observers in the sense that they do not write the legislation or collect the rents, but their interpretive posture (deference vs. enforcement of constitutional limits) structurally shapes the constraint's effective extraction. Their exit is analytical: they are bound by precedent and Article III constraints.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides ex ante incentive for creative investment by granting authors a time-limited exclusive right to control reproduction, distribution, adaptation, public performance, and display of their works. This solves the public goods problem of creative works (non-rival, non-excludable) by creating temporary excludability that enables market transactions.
% TRANSFER_FUNCTION: Moves economic value from users, follow-on creators, educators, archivists, and the public domain to corporate rights holders and collecting societies through: (1) extended monopoly terms that delay public domain entry, (2) anti-circumvention rules that prevent lawful format-shifting and interoperability, (3) narrowed fair use that forces licensing for transformative uses, (4) orphan works deadlock that makes unavailable works unusable. The transfer is from diffuse, often non-commercial actors to concentrated commercial entities.
% ABSENT_VOICES: The global south (countries pressured into TRIPS-plus enforcement via trade agreements), future generations (who inherit a depleted public domain), non-human creators (AI-generated works whose status is unresolved), and the vast majority of creators who never register copyright and gain no practical benefit from maximalist enforcement. These voices are structurally excluded: they lack standing in U.S. legislative and judicial processes, and their interests are not represented by any organized lobby with comparable resources to the entertainment coalition.
% DISAPPEARANCE_RATIONALE: If the corporate enclosure reading vanished overnight (replaced by the public scaffold reading with robust fair use, reasonable term limits, and orphan works solutions), the creative economy would reorganize: licensing markets would shrink, derivative creation would explode, archives would digitize freely, educational costs would drop, and corporate rights holders would lose a major rental stream. New business models would emerge around services, patronage, and voluntary collective licensing. The world would not stop creating — the incentive function would persist but the extraction layer would collapse.
% FOUNDING_PROBLEM: The Founders sought to solve the public goods problem of creative works in the early republic: authors had no practical protection against unauthorized printing, which discouraged investment in writing and publishing. The Constitution's Copyright Clause empowered Congress to grant temporary exclusive rights as an incentive mechanism — a utilitarian bargain, not a natural property right.
% FOUNDING_PROBLEM_CORROBORATION: The public scaffold reading's framing is corroborated by the constitutional text itself ('promote the Progress'), by Madison's Federalist No. 43 (copyright as 'a right of common law' but justified by 'the public good'), and by the 1790 Act's 14+14 year term — all from outside the corporate beneficiary set. The corporate enclosure reading's claim that the founding problem is 'still live' is corroborated only by the entertainment lobby's own testimony and commissioned studies. Independent economic historians (e.g., Boldrin & Levine, Kretschmer, Heald) document that the incentive function saturates at far shorter terms than current law provides, and that the public domain enrichment function was the dominant framing until the late 19th century corporate lobbying shift.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the reading captures value far beyond the incentive threshold: term extensions apply retroactively to existing works (no marginal incentive effect), anti-circumvention rules prevent lawful fair uses, and collecting societies extract from uses the statute does not clearly authorize. Suppression (0.72) is high because alternatives are actively suppressed: the public domain is frozen (no works entered 1998-2019), fair use is chilled by litigation risk and DMCA §1201, and orphan works cannot be used due to unidentifiable rights holders. Theater ratio (0.41) is moderate and rising: the coordination function (incentive) is real but a shrinking share of the constraint's operation; enforcement increasingly protects rental streams rather than incentivizing new creation. Accessibility collapse (0.68) reflects that alternatives (public domain, fair use, orphan works exceptions) have substantially collapsed for the victim groups. Resistance (0.55) is moderate: the constraint faces organized opposition (EFF, library associations, academics, some creators) but the beneficiaries wield superior legislative and litigation resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate rights holders and the entertainment lobby are structural beneficiaries (d ≈ 0.15): they collect the rental stream, write the legislative language, and control enforcement priorities. Collecting societies are secondary beneficiaries (d ≈ 0.25): they administer the extraction but depend on the statutory framework. Derivative creators, educators, archivists, and independent artists are structural targets (d ≈ 0.85): they bear the cost of clearance, licensing, and self-censorship; their exit options are constrained (identity_locked for artists whose practice depends on engagement with culture; trapped for archivists legally barred from preserving orphan works). The analytical observer seat (public interest advocates, some judges) sees the full structure (d = 0.5). The judicial ambiguity reading's rational-basis deference creates a structural ratchet: each extension resets the baseline, making the next extension easier to defend — this is the mechanism by which 'limited Times' becomes performative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (incentivizing creation by securing temporary monopoly) is contested: beneficiaries claim it remains live (piracy, digital copying); victims and independent scholars argue the problem is substantially solved for corporate incumbents and the arrangement now persists as rent collection. The constraint prevents mislabeling by exposing the coordination-extraction hybrid: without the genuine incentive function, this would be a pure snare; without the asymmetric extraction, it would be a rope. The tangled_rope classification captures that both are simultaneously true — the constraint coordinates AND extracts, and active enforcement (lobbying, litigation, DMCA rulemaking) is required to maintain the extraction layer. The mandatrophy resolution is that the coordination function has not atrophied (new works are still created under the regime) but the extraction layer has hypertrophied — the constraint is not a piton (the function is not dead) but a tangled_rope where the extraction tail wags the coordination dog.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the copyright constitutional mandate kernel, or a standalone constraint?',
    'Structural comparison with sibling readings: if the beneficiary/victim structure, epsilon, and foundational axioms differ systematically across readings, each reading instantiates a distinct constraint with its own epsilon (per ε-invariance principle).',
    'If distinct, the corporate enclosure reading carries its own classification (tangled_rope) and must not be averaged with the public scaffold reading (rope) or judicial ambiguity reading (scaffold). The kernel context documents the family relationship; the constraint itself remains ε-invariant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading is a distinct constraint instantiation of the copyright constitutional mandate kernel').

omega_variable(
    property_right_framing_naturalness,
    'Is the ''copyright as natural property right'' framing a genuine natural-law claim or a constructed cover story for corporate rent extraction?',
    'Historical genealogy of the property-right framing in Anglo-American copyright law: trace whether the natural-right rhetoric predates corporate lobbying structures or emerges alongside them. Compare with the public scaffold reading''s ''temporary monopoly for public good'' framing which has explicit constitutional textual support (Article I, Section 8, Clause 8).',
    'If the property-right framing is a constructed cover story, the constraint''s claimed natural-law foundation is a false summit — the beneficiary structure (corporate incumbents) and high extraction reveal it as a snare or tangled_rope masquerading as a natural entitlement. This routes through the false_summit_mountain signature if any party claims mountain status for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_right_framing_naturalness, conceptual, 'Natural-law vs. constructed ambiguity of the property-right framing for copyright').

omega_variable(
    limited_times_semantic_collapse,
    'Does ''limited times'' retain semantic content when interpreted as ''maximal extension short of perpetuity'', or has the constraint collapsed the constitutional limit into a performative gesture?',
    'Legislative history of term extensions (1976, 1998, and proposed further extensions): if each extension passes rational-basis review by treating ''limited'' as ''not literally infinite'', the constraint''s limiting function is performative. The Eldred v. Ashcroft dissent provides the structural counter-argument.',
    'If ''limited times'' has collapsed to performative gesture, the constraint''s coordination function (balancing incentive and access) is theater — the theater_ratio should rise toward 1.0 and the constraint reclassifies toward piton or snare. If the limit retains bite, the tangled_rope classification holds: real coordination (incentive) with real extraction (term capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limited_times_semantic_collapse, empirical, 'Whether the ''limited times'' constitutional limit has semantic content or has collapsed to performative theater').

omega_variable(
    fair_use_erosion_trajectory,
    'Is fair use doctrine being structurally narrowed as a downstream effect of the maximalist property-right framing?',
    'Track circuit court fair use decisions over time, particularly transformative use analysis after Campbell v. Acuff-Rose (1994) and the impact of DMCA anti-circumvention provisions on fair use exercise. Measure the gap between statutory fair use factors and litigated outcomes.',
    'If fair use is being structurally narrowed, the constraint''s extraction extends beyond term length to the very scope of permissible use — the victim set expands and effective extraction rises. This would increase the constraint''s measured extractiveness and suppression over time, potentially shifting classification from tangled_rope toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fair_use_erosion_trajectory, empirical, 'Whether fair use doctrine is eroding as a downstream consequence of maximalist copyright framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(copy_tr_t0, observed).
narrative_ontology:measurement(copy_tr_t22, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 22, 0.22).
narrative_ontology:measurement_basis(copy_tr_t22, observed).
narrative_ontology:measurement(copy_tr_t44, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 44, 0.33).
narrative_ontology:measurement_basis(copy_tr_t44, observed).
narrative_ontology:measurement(copy_tr_t66, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 66, 0.41).
narrative_ontology:measurement_basis(copy_tr_t66, observed).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(copy_be_t0, observed).
narrative_ontology:measurement(copy_be_t22, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 22, 0.61).
narrative_ontology:measurement_basis(copy_be_t22, observed).
narrative_ontology:measurement(copy_be_t44, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 44, 0.7).
narrative_ontology:measurement_basis(copy_be_t44, observed).
narrative_ontology:measurement(copy_be_t66, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 66, 0.78).
narrative_ontology:measurement_basis(copy_be_t66, observed).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(copy_su_t0, observed).
narrative_ontology:measurement(copy_su_t22, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 22, 0.55).
narrative_ontology:measurement_basis(copy_su_t22, observed).
narrative_ontology:measurement(copy_su_t44, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 44, 0.65).
narrative_ontology:measurement_basis(copy_su_t44, observed).
narrative_ontology:measurement(copy_su_t66, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 66, 0.72).
narrative_ontology:measurement_basis(copy_su_t66, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__corporate_enclosure_reading, 0.18).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, dmca_anti_circumvention_regime).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, orphan_works_deadlock).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the copyright_constitutional_mandate kernel. The corporate_enclosure_reading and public_scaffold_reading have ε values differing by ~0.53 (0.78 vs ~0.25) because they assess the SAME standing arrangement from structurally opposed framings. The judicial_ambiguity_reading sits between them structurally (ε ≈ 0.45) because its rational-basis deference creates a permissive envelope for the corporate reading's extensions while formally preserving the public scaffold's limiting principle. The three stories form a constraint family linked by network.affects_constraints. The corporate reading influences both siblings: it structurally pressures the judicial reading by creating legislative facts (extensions) that the rational-basis test must then defer to, and it pressures the public scaffold reading by shrinking the public domain the scaffold reading treats as the constraint's telos.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, institutional, 0.15).
constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, organized, 0.25).
constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, moderate, 0.8).
constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
