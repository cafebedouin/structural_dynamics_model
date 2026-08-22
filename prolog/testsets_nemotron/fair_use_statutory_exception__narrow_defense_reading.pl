% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense Preserving Copyright Market Value
 *   domain: intellectual_property/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   The narrow defense reading of fair use treats the statutory exception as
 *   a limited, defendant-burdened carve-out from a property-rule copyright
 *   system. It emerged not from the 1976 Act's text but from rights-holder
 *   litigation strategy (Harper & Row, Campbell, subsequent circuit
 *   decisions) that elevated 'market harm' to a presumptive fourth factor and
 *   made commercial nature nearly dispositive. The reading coordinates a
 *   licensing marketplace by making the license the default path for any use
 *   with a plausible market. Extraction is high because the constraint
 *   transfers value from a diffuse set of downstream users (creators,
 *   educators, archivists, remixers) to concentrated rights holders and
 *   intermediaries through licensing fees and litigation risk. Suppression is
 *   high because automated enforcement (Content ID, DMCA takedowns) and the
 *   cost of defending fair use claims eliminate alternatives for most users.
 *   Theater is moderate: the four-factor test is performed in courts and
 *   guidelines, but the factors are weighted to produce predictable 'no fair
 *   use' outcomes for commercial transformative works. The constraint is a
 *   tangled rope: it genuinely coordinates rights clearance for standard
 *   commercial exploitations (the rope function) while extracting from
 *   transformative and non-commercial uses that pose no market threat (the
 *   snare function).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.78).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.85).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense Preserving Copyright Market Value").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '94e96bbf-fd7b-430e-813a-581ea0fb01fb').
narrative_ontology:cs_kernel_codification('94e96bbf-fd7b-430e-813a-581ea0fb01fb', formalized).
narrative_ontology:cs_authority_grounding('94e96bbf-fd7b-430e-813a-581ea0fb01fb', lineage).
narrative_ontology:cs_interpretation_layer_present('94e96bbf-fd7b-430e-813a-581ea0fb01fb').
narrative_ontology:cs_reading_relation('94e96bbf-fd7b-430e-813a-581ea0fb01fb', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('94e96bbf-fd7b-430e-813a-581ea0fb01fb', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('94e96bbf-fd7b-430e-813a-581ea0fb01fb', foundational, copyright_as_property_right_absolute).
narrative_ontology:cs_axiom_status(copyright_as_property_right_absolute, holdable).
narrative_ontology:cs_axiom_grounding('94e96bbf-fd7b-430e-813a-581ea0fb01fb', copyright_as_property_right_absolute, deontological).
narrative_ontology:cs_axiom('94e96bbf-fd7b-430e-813a-581ea0fb01fb', foundational, market_harm_presumption_for_commercial_use).
narrative_ontology:cs_axiom_status(market_harm_presumption_for_commercial_use, holdable).
narrative_ontology:cs_axiom_grounding('94e96bbf-fd7b-430e-813a-581ea0fb01fb', market_harm_presumption_for_commercial_use, instrumental).
narrative_ontology:cs_axiom('94e96bbf-fd7b-430e-813a-581ea0fb01fb', secondary, affirmative_defense_burden_on_defendant).
narrative_ontology:cs_axiom_status(affirmative_defense_burden_on_defendant, holdable).
narrative_ontology:cs_axiom_grounding('94e96bbf-fd7b-430e-813a-581ea0fb01fb', affirmative_defense_burden_on_defendant, conventional).
narrative_ontology:cs_reference_frame('94e96bbf-fd7b-430e-813a-581ea0fb01fb', statutory_fair_use_1976_codification).
narrative_ontology:cs_drift_state('94e96bbf-fd7b-430e-813a-581ea0fb01fb', post_campbell_digital_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('94e96bbf-fd7b-430e-813a-581ea0fb01fb', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, licensing_intermediaries).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, content_industry_associations).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, creators_of_derivative_works).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, digital_archivists).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, small_scale_remix_artists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, copyright_as_property_right).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, market_harm_presumption).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, affirmative_defense_burden_on_defendant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive rights over creative works and extract licensing revenue from downstream uses. Benefit from the narrow defense reading because it shifts the burden to defendants to prove their use is fair, making licensing the default path for most commercial uses. Can enforce through litigation threats and automated takedown systems.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Operate collective licensing societies, stock agencies, and rights clearance platforms. Collect transaction fees from every licensed use. The narrow defense reading expands the universe of uses requiring licenses, directly growing their revenue base. They lobby for interpretive guidance that treats licensing availability as evidence against fair use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, licensing_intermediaries, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, licensing_intermediaries, agenda_setter).

% Trade groups (RIAA, MPAA, AAP, etc.) that shape legislative amendments, file amicus briefs, and fund litigation establishing precedent. They advance the property-right framing and market-harm presumption through coordinated legal strategy. Their agenda-setting power derives from concentration of industry resources and access to policymakers.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, content_industry_associations, agenda_setter,
    institutional, generational, arbitrage, global).

% Artists, writers, musicians, and developers who build on existing works. Under the narrow defense, they face high legal risk for transformative uses that don't fit rigid categories. Licensing costs are often prohibitive or rights holders are unidentifiable. Exit means abandoning creative practice or moving to jurisdictions with broader exceptions — both costly.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, creators_of_derivative_works, payer,
    moderate, biographical, constrained, global).

% Universities, schools, and libraries that rely on fair use for teaching, research, and preservation. They benefit from some settled educational guidelines but face escalating licensing demands for digital and distance education. The narrow defense forces them into blanket licenses that exceed fair use value. Exit is constrained by accreditation requirements and mission obligations.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, educational_institutions, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, educational_institutions, beneficiary).

% Documentarians routinely incorporate copyrighted material (footage, music, art) incidentally or critically. The narrow defense treats incidental capture as infringement unless cleared. Errors & omissions insurance requires clearance for all recognizable content, forcing payment for uses that would be fair under broader readings. Exit means self-censorship or abandoning subjects.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, global).

% Libraries, archives, and preservationists digitizing at-risk cultural materials. Orphan works and unclear rights make licensing impossible at scale. The narrow defense offers no safe harbor for preservation copying or access provision. They operate in a legal gray zone, risking statutory damages. Exit is structurally blocked — preservation is their mandate, but the law gives no clear path.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, digital_archivists, payer,
    moderate, generational, trapped, global).

% Meme creators, fan fiction writers, sampling musicians, and social media content producers. Their practice is constitutive of their creative identity — they cannot 'exit' to non-remix creation without abandoning their voice. The narrow defense treats their work as presumptively infringing. Platform takedown systems enforce this at scale with no fair use review. They bear the full cost of the constraint's suppression.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, small_scale_remix_artists, payer,
    powerless, immediate, identity_locked, global).

% Federal courts (especially appellate) that interpret and apply the four-factor test. Their precedent-setting decisions determine whether the narrow defense reading or a broader reading prevails in practice. They are structurally positioned to observe the constraint's operation but are bound by stare decisis and the statutory text.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts_judiciary, observer,
    institutional, generational, analytical, national).

% Academics who analyze fair use doctrine, propose frameworks, and file amicus briefs. They map the constraint's effects across stakeholder seats but lack enforcement power. Their analyses influence judicial reasoning over long time horizons. Some advocate for the narrow defense; most critique it as over-restrictive.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, legal_scholarship_community, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, property-rule framework for allocating rights in expressive works: creators get exclusive control; users negotiate licenses. The narrow defense reading coordinates by making the license the default and the exception narrow, reducing transaction costs for rights holders at the expense of user certainty.
% TRANSFER_FUNCTION: Moves licensing revenue and legal risk from copyright holders and intermediaries to downstream users. Every use that could theoretically be licensed becomes a transfer occasion — the user pays or bears litigation risk. The burden of proof on the defendant (affirmative defense) operationalizes this transfer.
% ABSENT_VOICES: Future creators whose work would build on today's culture but cannot yet speak; users in jurisdictions without fair use equivalents who have no exception at all; public domain advocates who argue the constraint shrinks the cultural commons. These voices are structurally excluded from the licensing marketplace and the courtroom where precedent is set.
% DISAPPEARANCE_RATIONALE: If the narrow defense reading vanished overnight and was replaced by a broader transformative-use standard, licensing markets would contract for transformative works, documentary and remix production would surge, educational and archival practices would normalize without blanket licenses, and copyright holder revenue would shift from broad licensing to core commercial exploitations. The creative ecosystem would reorganize around permissionless innovation for transformative uses.
% FOUNDING_PROBLEM: Early U.S. copyright law granted narrow rights (printing, reprinting). As reproduction technologies multiplied (photography, phonorecords, film, software), courts and Congress expanded the exclusive rights bundle. The 'fair use' codification in 1976 was a compromise: it preserved judicial flexibility but did not define the exception's scope. The narrow defense reading emerged from rights holder litigation strategy in the 1980s-90s (Sony, Harper & Row, Campbell) to cabin the exception and protect emerging licensing markets.
% FOUNDING_PROBLEM_CORROBORATION: Rights holder groups attest the problem (unauthorized copying displacing sales) is live and growing with digital distribution. Legal historians (Patry, Litman, Samuelson) and empirical studies (Heller & Eisenberg on anticommons; Heald on orphan works) attest the founding problem has mutated: the constraint now suppresses uses that don't displace markets and creates orphan works gridlock. No consensus exists — the status is genuinely contested.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) reflects the scale of value transfer: licensing markets for transformative/educational/archival uses that would be fair under broader readings now generate billions in revenue for rights holders and intermediaries. Suppression (0.85) reflects the combination of statutory damages (up to $150k/work), the affirmative defense burden, and automated takedown systems that make resistance prohibitively expensive for all but the largest users. Theater (0.42) reflects that the four-factor test is real doctrine but operates as a ritual — factor one (commercial nature) and factor four (market harm) are weighted so heavily that the analysis is often predetermined. Accessibility collapse (0.68) reflects that alternatives (licensing, public domain, de minimis) are structurally available but practically closed for orphan works, incidental uses, and low-budget creators. Resistance (0.55) reflects sustained pushback from academia, libraries, tech platforms, and creator communities — but resistance has not shifted the doctrinal center of gravity.
 *
 * PERSPECTIVAL GAP:
 *   From the copyright holder/licensing intermediary seat, the constraint is a rope: it enables efficient rights clearance and predictable revenue. From the documentary filmmaker/digital archivist/remix artist seat, it is a snare: it suppresses their practice without compensating them. From the educational institution seat, it is a tangled rope: they get some coordination benefit (guidelines for classroom copying) but pay escalating extraction (digital course licenses). The engine computes this seat divergence from the structural power/exit asymmetries — the agenda setters (industry associations) have arbitrage-grade exit (they shape the law), while small-scale remix artists are identity-locked (their creative practice IS the constrained activity).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (copyright holders, licensing intermediaries, industry associations) collect licensing revenue and control the enforcement agenda — their directionality is near 0.0 (full beneficiary). Victims (derivative creators, educators, documentarians, archivists, remix artists) bear licensing costs, litigation risk, and creative suppression — their directionality is near 1.0 (full target). The dual-role stakeholders (educational institutions as both payer and beneficiary; small remix artists as identity-locked payers) sit at intermediate d values reflecting their mixed position. Courts and scholars are analytical observers (d=0.5 by construction).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (balancing creator incentive with public access) has atrophied into a licensing maximization regime. The founding problem (preventing market displacement) is contested — displacement still occurs, but the constraint now captures value from non-displacing uses. The narrow defense reading resolves mandatrophy by pretending the tradeoff doesn't exist: it treats every unlicensed use as a market harm, converting a coordination problem (how to allocate rights efficiently) into an extraction mechanism (pay for everything or stop). The classification as tangled rope captures this duality: the coordination function (standard licensing) is real but the extraction function (suppressing transformative uses) is dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the narrow defense reading a faithful interpretation of the 1976 Act''s legislative history, or a judicial construction that displaces the statute''s deliberate ambiguity?',
    'Historical analysis of Congressional reports, floor debates, and the CONTU process; comparison with the House/Senate report language on ''breathing space'' for users.',
    'If the reading is a judicial construction, its legitimacy depends on stare decisis, not statutory fidelity — making it more vulnerable to legislative correction or Supreme Court revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the narrow defense reading is textual or constructed').

omega_variable(
    market_harm_presumption_validity,
    'Does the presumption that commercial transformative uses harm the market for the original hold empirically, or does it reflect a theoretical monopoly-rent model?',
    'Empirical studies of licensing markets for transformative uses (parody, criticism, documentary incorporation, sampling) — do rights holders actually license these uses, or does the presumption create a market that wouldn''t exist?',
    'If the presumption creates its own market (anticommons), the extraction is circular: the constraint creates the ''harm'' it then uses to justify extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_harm_presumption_validity, empirical, 'Whether market harm presumption is empirically grounded or self-fulfilling').

omega_variable(
    transformative_use_measurement,
    'Can ''transformativeness'' be measured consistently enough to serve as a doctrinal pivot, or is it inherently indeterminate?',
    'Inter-coder reliability studies on transformative use coding; analysis of circuit split patterns post-Campbell; computational text/image analysis of transformative vs. derivative works.',
    'If transformativeness is measurably incoherent, the narrow defense''s rejection of it as a primary factor is pragmatically justified; if it is measurable, the narrow defense''s underweighting is a policy choice, not a necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_measurement, empirical, 'Whether transformativeness is a workable doctrinal standard').

omega_variable(
    orphan_works_extraction,
    'How much of the measured extraction comes from uses where no rights holder can be identified or located (orphan works), vs. uses where licensing is possible but refused?',
    'Copyright Office orphan works studies; empirical surveys of failed clearance attempts; analysis of statutory damages awards in orphan works contexts.',
    'If a large share of extraction is from orphan works, the constraint extracts from users who have no counterparty to negotiate with — pure suppression without coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(orphan_works_extraction, empirical, 'Proportion of extraction attributable to orphan works vs. negotiable licenses').

omega_variable(
    automated_enforcement_fair_use_blindness,
    'Do automated content identification systems (Content ID, Audible Magic, etc.) have any capacity to recognize fair use, or do they structurally enforce the narrow defense by default?',
    'Technical audit of matching algorithms; analysis of dispute/appeal rates and outcomes; platform transparency reports on fair use consideration in automated systems.',
    'If automated systems cannot recognize fair use, the suppression metric understates the constraint''s actual operation — the effective suppression for small users is near 1.0 regardless of the legal standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(automated_enforcement_fair_use_blindness, empirical, 'Whether automated enforcement systems incorporate fair use analysis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1976, 0.15).
narrative_ontology:measurement(fair_tr_t1985, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(fair_tr_t1994, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1994, 0.3).
narrative_ontology:measurement(fair_tr_t1998, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(fair_tr_t2005, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(fair_tr_t2015, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(fair_tr_t2026, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement(fair_be_t1985, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(fair_be_t1994, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1994, 0.58).
narrative_ontology:measurement(fair_be_t1998, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1998, 0.65).
narrative_ontology:measurement(fair_be_t2005, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2005, 0.71).
narrative_ontology:measurement(fair_be_t2015, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(fair_be_t2026, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1976, 0.45).
narrative_ontology:measurement(fair_su_t1985, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(fair_su_t1994, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1994, 0.68).
narrative_ontology:measurement(fair_su_t1998, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1998, 0.75).
narrative_ontology:measurement(fair_su_t2005, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(fair_su_t2015, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2015, 0.83).
narrative_ontology:measurement(fair_su_t2026, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2026, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__narrow_defense_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, dmca_takedown_regime).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, orphan_works_gridlock).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, statutory_damages_framework).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, content_id_automated_enforcement).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__market_licensing_reading).

% DUAL FORMULATION NOTE:
% The fair_use_statutory_exception kernel decomposes into three constraint stories: narrow_defense_reading (this story, high ε, tangled rope), transformative_right_reading (lower ε for transformative uses, rope/tangled rope boundary), and market_licensing_reading (high ε for any licensable use, snare). The narrow defense provides the doctrinal infrastructure (market harm presumption, commerciality weight) that the market licensing reading operationalizes. The transformative right reading is the primary counter-reading that limits extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, organized, 0.35).
constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, powerless, 0.95).
constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
