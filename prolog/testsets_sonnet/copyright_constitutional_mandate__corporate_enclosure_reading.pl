% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Maximal Property Right (Corporate Enclosure Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the corporate-enclosure reading of the copyright
 *   constitutional kernel: the position that copyright is fundamentally a
 *   property right entitled to maximal protection, and that the
 *   constitutional phrase 'limited times' permits any extension short of
 *   explicit perpetuity. Under this reading, the 1976 Act, the 1998 Sonny
 *   Bono Copyright Term Extension Act, and the DMCA's anti-circumvention
 *   provisions are the correct and expected operation of the clause, not a
 *   drift from it. The reading is authored here on its own terms —
 *   coordination function, beneficiaries, victims, and enforcement machinery
 *   are those consistent with treating maximal protection as the
 *   constitutional mandate. Two sibling constraints instantiate the other
 *   readings of the same kernel text: judicial_ambiguity_reading (the courts
 *   defer to legislative discretion without endorsing either substantive
 *   theory) and public_scaffold_reading (copyright exists instrumentally to
 *   enrich the public domain, monopoly being temporary means to that end).
 *   This story's epsilon is high and stable-to-rising because the enclosure
 *   reading, taken as the operative constitutional theory, has no internal
 *   limiting principle on term length short of explicit perpetuity — each
 *   extension is consistent with, not a violation of, the reading's own
 *   premise.
 *
 * KEY AGENTS:
 *   - legacy_media_conglomerates: Primary beneficiary and agenda-setter (institutional/arbitrage) — collects licensing rents and drives extension legislation
 *   - derivative_creators, educators, archivists, public_domain_researchers, independent_remix_artists: Primary targets (moderate-to-powerless/constrained-to-trapped) — bear licensing costs, legal risk, and loss of anticipated public-domain entry
 *   - congress: Agenda-setter that enacts the extensions industry lobbies for
 *   - judiciary: Analytical observer whose rational-basis deference is the mechanism enabling this reading to operate unchecked
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.76).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Maximal Property Right (Corporate Enclosure Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, 'ddf3684c-afc6-4295-8f2e-8a749c49cb9e').
narrative_ontology:cs_kernel_codification('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', fixed_text).
narrative_ontology:cs_authority_grounding('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', extraction).
narrative_ontology:cs_interpretation_layer_present('ddf3684c-afc6-4295-8f2e-8a749c49cb9e').
narrative_ontology:cs_reading_relation('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', foundational, copyright_is_natural_property_entitled_to_maximal_protection).
narrative_ontology:cs_axiom_status(copyright_is_natural_property_entitled_to_maximal_protection, holdable).
narrative_ontology:cs_axiom_grounding('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', copyright_is_natural_property_entitled_to_maximal_protection, deontological).
narrative_ontology:cs_axiom('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', foundational, limited_times_imposes_no_substantive_ceiling_short_of_perpetuity).
narrative_ontology:cs_axiom_status(limited_times_imposes_no_substantive_ceiling_short_of_perpetuity, holdable).
narrative_ontology:cs_axiom_grounding('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', limited_times_imposes_no_substantive_ceiling_short_of_perpetuity, conventional).
narrative_ontology:cs_reference_frame('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', founders_limited_monopoly_bargain).
narrative_ontology:cs_drift_state('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', post_sonny_bono_dmca_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ddf3684c-afc6-4295-8f2e-8a749c49cb9e', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_media_conglomerates).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, music_licensing_organizations).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, motion_picture_trade_associations).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, long_lived_character_ip_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_researchers).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, independent_remix_artists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold copyrights on characters and works whose commercial value persists for a century or more. Lobby directly for term extensions timed to expirations of flagship properties, fund model legislation, and litigate to expand what counts as infringement. Extract licensing revenue from every derivative use they can locate and criminally pursue circumvention of access controls on their catalogs.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_media_conglomerates, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_media_conglomerates, agenda_setter).

% Administer licensing on behalf of rights-holders and collect fees system-wide; benefit from term length and enforcement intensity regardless of whether the individual creators they represent see proportional returns. Their institutional survival depends on the enforcement apparatus remaining strong.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, music_licensing_organizations, beneficiary,
    organized, generational, arbitrage, global).

% Coordinate industry-wide lobbying for extension and anti-circumvention statutes, draft model international treaty language, and fund enforcement litigation against streaming and archival services. Set the legislative agenda that legislators subsequently ratify.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, motion_picture_trade_associations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, motion_picture_trade_associations, agenda_setter).

% Want to build new work — fan fiction, sampling, remix, adaptation — on top of existing cultural material. Face licensing costs that scale with the fame of the underlying work and criminal liability for circumventing digital locks even for otherwise lawful transformative use. Their practical exit is abandoning the derivative project or operating underground.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, national).

% Need to reproduce, excerpt, and adapt copyrighted materials for classroom use. Face an expanding zone of infringement risk as fair use is narrowed in litigation and licensing markets colonize uses once treated as free. Institutional risk-aversion (schools, libraries) often means self-censorship well beyond the legal minimum.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    moderate, biographical, constrained, national).

% Attempt to preserve and provide access to at-risk cultural works — film, software, out-of-print books. Extended terms mean orphan works with no findable rights-holder remain legally frozen for decades past any plausible commercial value, and anti-circumvention rules can make preservation copying itself a crime.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    moderate, generational, trapped, national).

% Study, catalog, and build on works that would otherwise be entering the public domain. Each term extension retroactively removes works from the pool they were counting on, with no notice or compensation, and no seat at the legislative table where the extension is negotiated.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_researchers, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_researchers, excluded).

% Produce transformative digital work — mashups, machinima, sampled music — that platforms and rights-holders flag or take down automatically. Cannot afford litigation to test fair use defenses and so treat the boundary as wherever the automated enforcement system draws it, which is drawn conservatively in the rights-holder's favor.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, independent_remix_artists, payer,
    powerless, biographical, trapped, global).

% Enacts term-extension and anti-circumvention statutes, historically timed closely to industry lobbying cycles and expirations of high-value properties. Reviews the constitutional 'limited times' language but has repeatedly extended term length rather than letting works enter the public domain on the original schedule.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% Reviews term-extension statutes under rational basis review and has upheld them (Eldred v. Ashcroft) rather than enforcing a strict reading of 'limited times.' Its deference is the mechanism that allows this reading's expansive claim to operate without constitutional check.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, internationally-recognized property claim that lets rights-holders plan long-horizon investment in content production and licensing infrastructure without fear of expropriation by imitators.
% TRANSFER_FUNCTION: Moves licensing revenue, control over derivative use, and access to cultural raw material from downstream creators, educators, and archivists to the holders of long-lived copyrights — extended repeatedly each time flagship properties near expiration.
% ABSENT_VOICES: The public domain itself has no lobbyist. Future creators who would have built freely on works now retroactively re-enclosed were never represented in the legislative process that extended the term against them; archivists warning of orphan-works loss testified but were structurally outweighed by concentrated industry lobbying.
% DISAPPEARANCE_RATIONALE: If this reading's maximal-protection claim were abandoned in favor of a strict limited-times reading, a large body of 20th-century culture would immediately enter the public domain, licensing markets built on extended terms would need to be restructured, and enforcement budgets currently spent chasing circumvention and fair-use edge cases would be redirected or eliminated.
% FOUNDING_PROBLEM: The constitutional copyright clause was framed to solve a genuine problem: without any exclusive right, authors and publishers might underinvest in producing and distributing new works because imitators could free-ride immediately. A time-limited monopoly was the proposed fix.
% FOUNDING_PROBLEM_CORROBORATION: Rights-holder trade associations attest the incentive problem remains live and requires strong, long-duration protection. Independent legal scholars (e.g., in amicus filings in Eldred v. Ashcroft), archivists' professional associations, and public-domain advocacy organizations attest that for the vast majority of works commercial value is exhausted within a few decades, so the incentive rationale does not justify terms now running past a century — corroboration exists from outside the beneficiary set and directly disputes this reading's premise.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is high (0.81) and has risen steadily across every major legislative extension (1976, 1998) and enforcement expansion (DMCA 1998, subsequent circumvention prosecutions) because each event moved value from downstream users to incumbent rights-holders without a corresponding new coordination benefit — the works already existed; only the exclusivity window lengthened. Suppression (0.76) is authored high because anti-circumvention provisions carry criminal liability, and enforcement (automated takedown systems, DMCA notice regimes) operates independent of whether the underlying use would ultimately be found fair. Theater ratio (0.42) reflects that some of the apparatus (public registration, notice-and-takedown counter-notice procedures) performs due process without altering outcomes for parties who cannot afford litigation. Accessibility collapse (0.62) and resistance (0.58) are both substantial but not maximal: alternative distribution and creative commons licensing persist as partial outlets, and there is real organized resistance (Public Knowledge, EFF, library associations) — this is not a mountain, it meets active contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Legacy media conglomerates, music licensing organizations, and motion picture trade associations sit at the beneficiary end: they collect licensing revenue, set the legislative agenda through concentrated lobbying, and have durable institutional exit options (they can relocate IP holding companies, litigate strategically, or shape treaty language). Derivative creators, educators, archivists, public domain researchers, and independent remix artists sit at the target end: they bear licensing costs and legal risk, several are structurally trapped (archivists cannot simply exit their preservation mission; remix artists cannot exit platform enforcement systems), and none has comparable access to the legislative process. Congress functions as agenda-setter but is itself substantially responsive to the concentrated beneficiary lobby, which is why term extension has consistently outpaced any renewed coordination rationale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — underinvestment absent any exclusive right — is real but time-bound: economic evidence on optimal copyright term consistently finds commercial value for the overwhelming majority of works exhausted within a few decades, not a century-plus. This reading's classification as tangled_rope (not snare) preserves the genuine coordination function (some fixed-term exclusivity does incentivize investment) while registering that the SPECIFIC extension pattern this reading endorses has decoupled from that founding rationale — extraction now substantially exceeds what the coordination problem requires, which is exactly the tangled-rope signature: real coordination function plus asymmetric extraction riding on the same structure, sustained only by active enforcement (DMCA criminal provisions, platform takedown mandates, international treaty harmonization pressure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    limited_times_textual_ceiling,
    'Does the constitutional phrase ''limited times'' impose any substantive ceiling on term length, or is any term short of explicit perpetuity constitutionally permissible?',
    'A future Supreme Court ruling squarely addressing whether repeated, foreseeable term extensions functionally equivalent to perpetuity violate the clause, going beyond the rational-basis deference given in Eldred v. Ashcroft (2003).',
    'If a substantive ceiling is found, this reading is foreclosed and the public_scaffold_reading or judicial_ambiguity_reading becomes the operative constitutional theory; if no ceiling is found, this reading''s premise is judicially vindicated and further extension remains unconstrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limited_times_textual_ceiling, conceptual, 'Whether ''limited times'' has a substantive or merely formal limiting function.').

omega_variable(
    corporate_enclosure_vs_natural_property_claim,
    'Is copyright genuinely analogous to natural property (justifying maximal protection by default) or is it a purely statutory, instrumentally-created monopoly (justifying protection only to the extent the coordination rationale requires)?',
    'Comparative legal-philosophical analysis of the historical record (statute of Anne, framers'' debates) versus the natural-rights property tradition; economic analysis of whether term length beyond ~50 years produces measurable additional incentive to create.',
    'If copyright is genuinely property-like, the enclosure reading''s premise holds and current extraction levels are the correct operation of a property right, not extraction at all. If copyright is purely instrumental, extraction beyond the coordination-justified term is properly registered as excess independent of any property analogy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_enclosure_vs_natural_property_claim, conceptual, 'Whether the property-right framing itself is the correct lens for evaluating extraction, or a rhetorical move that pre-loads the answer.').

omega_variable(
    lobbying_capture_vs_genuine_deliberation,
    'Were the 1976 and 1998 term extensions the product of genuine congressional deliberation about optimal incentive structure, or were they substantially the product of concentrated industry lobbying timed to specific expiring properties?',
    'Legislative history analysis: lobbying expenditure records, timing correlation between extension enactment and expiration dates of high-value copyrighted characters, comparison of congressional testimony from industry versus independent economists.',
    'If capture-driven, the extension pattern under this reading is better understood as regulatory capture riding on a property-rights rhetoric, strengthening the tangled_rope classification. If genuinely deliberative, the extensions reflect considered policy judgment consistent with the reading''s own account of itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lobbying_capture_vs_genuine_deliberation, empirical, 'Whether documented lobbying timing corroborates or undercuts the reading''s self-account.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(copy_tr_t1988, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1988, 0.24).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1998, 0.3).
narrative_ontology:measurement(copy_tr_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(copy_tr_t2017, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2017, 0.39).
narrative_ontology:measurement(copy_tr_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.48).
narrative_ontology:measurement(copy_be_t1988, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1988, 0.55).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(copy_be_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2008, 0.74).
narrative_ontology:measurement(copy_be_t2017, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2017, 0.78).
narrative_ontology:measurement(copy_be_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2026, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(copy_su_t1988, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1988, 0.48).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1998, 0.62).
narrative_ontology:measurement(copy_su_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(copy_su_t2017, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2017, 0.72).
narrative_ontology:measurement(copy_su_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2026, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__corporate_enclosure_reading, 0.1).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, dmca_anticircumvention_enforcement).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the copyright_constitutional_mandate kernel. corporate_enclosure_reading (this file) treats 'limited times' as permitting maximal extension and authors a tangled_rope classification with high, rising extraction. judicial_ambiguity_reading treats the same text as committing courts to rational-basis deference without endorsing a substantive theory, and is expected to show a lower, more stable epsilon reflecting genuine institutional uncertainty rather than settled extraction. public_scaffold_reading treats copyright as instrumentally justified only by public-domain enrichment, and is expected to classify closer to scaffold (with a sunset-clause-consistent structure) given its premise that the monopoly is explicitly temporary means to a public good end. All three share the same constitutional text and beneficiary/victim raw material but diverge sharply in epsilon and type because they instantiate different normative readings of the same kernel, per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
