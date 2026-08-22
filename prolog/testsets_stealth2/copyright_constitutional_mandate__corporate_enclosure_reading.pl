% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Maximal Property Entitlement ('Limited Times' Construed as Any Finite Bound)
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The standing arrangement under contest: statutory protection running
 *   life-plus-seventy for authors and ninety-five to one-hundred-twenty years
 *   for corporate works, anti-circumvention liability decoupled from any act
 *   of infringement, and a fair-use doctrine narrowed in practice by
 *   clearance economics and automated filtering. The governing interpretive
 *   commitment reads copyright as a property entitlement requiring maximal
 *   protection, with the constitutional 'limited Times' qualifier construed
 *   as satisfied by any finite bound however long. Assumption: interval index
 *   0-30 maps to calendar years 1995-2025, spanning the term-extension and
 *   enforcement build-out (t=3: the 1998 term-extension and
 *   anti-circumvention statutes; t=8: Eldred upholds extension; t=17: Golan
 *   restores removed works; t=24: public-domain entry resumes as the first
 *   frozen cohort expires; t=29-30: institutional-lender litigation and the
 *   first iconic expiries). KEY AGENTS (by structural relationship): -
 *   major_studio_rights_holders: Primary beneficiary and co-agenda-setter
 *   (institutional/arbitrage) — collects catalog rents, drafts the extensions
 *   - recorded_music_incumbents, legacy_publishing_conglomerates: Secondary
 *   beneficiaries (institutional/arbitrage) - drm_enforcement_vendors:
 *   Enforcement-economy beneficiary (institutional/mobile) -
 *   congressional_ip_committees, federal_appellate_courts,
 *   us_trade_representative: Administrator seats (institutional/constrained)
 *   — legislate, ratify, and export the terms - derivative_creators,
 *   educators_and_researchers: Primary targets (moderate/constrained) — bear
 *   clearance and liability costs - libraries_archives_museums: Target with
 *   mission-fused exit (institutional/identity_locked) -
 *   general_culture_consumers, future_creator_generations: Diffuse targets
 *   (powerless/trapped) - digital_rights_advocacy_organizations: Effectively
 *   excluded participant (organized/constrained) - trading_partner_nations:
 *   Harmonized-cost bearer, excluded from agenda-setting
 *   (organized/constrained)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.8).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.76).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, snare).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Maximal Property Entitlement ('Limited Times' Construed as Any Finite Bound)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd').
narrative_ontology:cs_kernel_codification('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', fixed_text).
narrative_ontology:cs_authority_grounding('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', lineage).
narrative_ontology:cs_interpretation_layer_present('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd').
narrative_ontology:cs_reading_relation('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', foundational, copyright_is_natural_property_entitlement).
narrative_ontology:cs_axiom_status(copyright_is_natural_property_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', copyright_is_natural_property_entitlement, deontological).
narrative_ontology:cs_axiom('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', foundational, limited_times_permits_maximal_finite_duration).
narrative_ontology:cs_axiom_status(limited_times_permits_maximal_finite_duration, holdable).
narrative_ontology:cs_axiom_grounding('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', limited_times_permits_maximal_finite_duration, conventional).
narrative_ontology:cs_axiom('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', secondary, protection_intensity_tracks_property_status).
narrative_ontology:cs_axiom_status(protection_intensity_tracks_property_status, holdable).
narrative_ontology:cs_axiom_grounding('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', protection_intensity_tracks_property_status, instrumental).
narrative_ontology:cs_reference_frame('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', copyright_as_property_entitlement).
narrative_ontology:cs_drift_state('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', contemporary_post_ctea_enforcement_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('6e6c14b1-0772-4aaf-9ac9-83d86a8ef4bd', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, major_studio_rights_holders).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, recorded_music_incumbents).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_publishing_conglomerates).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, drm_enforcement_vendors).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators_and_researchers).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, libraries_archives_museums).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, general_culture_consumers).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, future_creator_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, trading_partner_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own century-deep catalogs of films, characters, and recordings whose value compounds the longer protection lasts. Fund the trade associations and campaign committees that draft and press term-extension bills, and license the catalog through every channel. Holdings can be restructured across jurisdictions and affiliate markets, so no single country's rules bind them.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, major_studio_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, major_studio_rights_holders, agenda_setter).

% Control master recordings and publishing catalogs; revenue arrives as streaming and synchronization licenses priced under the long term. Pressed for the term extensions that kept mid-century recordings out of the public domain and for expanding performance-rights collections. Portfolios can be sold, moved, or re-domiciled at will.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, recorded_music_incumbents, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, recorded_music_incumbents, agenda_setter).

% Hold backlists of books and journals; income depends on controlling reprint, course-pack, and translation rights for as long as possible. Support enforcement harmonization through international publishers' associations and domestic lobbying.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_publishing_conglomerates, beneficiary,
    institutional, generational, arbitrage, global).

% Sell the technical protection stack — encryption, watermarking, takedown tooling, automated content-matching systems — whose market exists because circumvention is unlawful and platforms must filter. Revenue scales with the breadth of what counts as protected.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, drm_enforcement_vendors, beneficiary,
    institutional, biographical, mobile, global).

% Draft and pass the term extensions and enforcement titles. Receive campaign contributions and employment-constituency arguments concentrated in media-heavy districts; committee jurisdiction and electoral financing depend on continuing relationships with the industries being regulated.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, congressional_ip_committees, agenda_setter,
    institutional, biographical, constrained, national).

% Review challenges to term extensions and enforcement statutes; have upheld them under deferential review, treating term length as a legislative judgment. Bound by precedent and doctrine; cannot initiate revision, only ratify or strike what arrives.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, federal_appellate_courts, agenda_setter,
    institutional, generational, constrained, national).

% Negotiates bilateral and multilateral intellectual-property chapters that export domestic term lengths and enforcement standards, and administers watch-list pressure on trading partners. Operates through industry advisory committees with privileged drafting access.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, us_trade_representative, agenda_setter,
    institutional, biographical, constrained, global).

% Documentary filmmakers, remix artists, sampling musicians, and fan creators build on existing works and face clearance costs, takedown notices, and statutory-damage exposure that scale with how long protection runs and how broadly it reaches. Fair-use counsel and errors-and-omissions insurance help, but many projects die in clearance.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, global).

% Need excerpts, images, films, and datasets for teaching and scholarship; navigate case-by-case fair-use judgment, licensed courseware fees, and takedown risk on posted materials. Budgets are fixed while license prices compound with term length.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators_and_researchers, payer,
    moderate, biographical, constrained, global).

% Preserve and lend cultural material under narrow statutory exceptions; preserving software, e-books, and obsolete media often requires breaking technical locks that anti-circumvention law forbids breaking even for lawful purposes. Permanent-access missions make retreat from acquisition and preservation untenable, and litigation has been brought against their lending programs directly.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, libraries_archives_museums, payer,
    institutional, generational, identity_locked, global).

% Pay embedded licensing costs in tickets, subscriptions, and goods, and wait decades longer than the founding generation's fourteen-year terms for works to become freely usable. There is no opting out of the protection regime surrounding the culture they consume.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, general_culture_consumers, payer,
    powerless, biographical, trapped, global).

% Will inherit a public domain whose twentieth-century layer stays closed longest, building on a thinner commons than any prior cohort received. They have no representative in any current proceeding.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, future_creator_generations, payer,
    powerless, generational, trapped, global).

% Litigate, publish, and mobilize against term extension and enforcement expansion. They hold formal participatory standing through agency comments and amicus briefs, yet the enacted record across three decades contains no term reduction and no broadened exception their agenda produced; their substantive proposals sit outside the operative legislative conversation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, digital_rights_advocacy_organizations, excluded,
    organized, biographical, constrained, global).

% Accept term lengths and enforcement standards drafted elsewhere through treaty accession and watch-list pressure. Domestic users, libraries, and educators bear the harmonized costs while their own balancing traditions — broader exceptions, shorter terms — are negotiated away without domestic agenda-setting.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, trading_partner_nations, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, trading_partner_nations, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__corporate_enclosure_reading, major_studio_rights_holders).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__corporate_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees a return window for expensive-to-produce, cheap-to-copy works, coordinating investment in films, recordings, and publications; standardized ownership records also make large-scale licensing markets tractable. Under this reading the window is construed as long as constitutionally expressible.
% TRANSFER_FUNCTION: Moves licensing revenue, catalog control, and would-be public-domain value from the general public, derivative creators, educators, and archivists to incumbent rights holders; moves compliance, clearance, and litigation-risk costs onto secondary users and cultural institutions.
% ABSENT_VOICES: Future creator generations and the future public-domain audience have no seat anywhere in the process — no one represents the readers of 2090 in a 1998 hearing. Unlicensed derivative communities and informal archivists appear chiefly as defendants. Trading-partner publics receive the harmonized rules through executives and trade ministries rather than through their own legislative balancing.
% DISAPPEARANCE_RATIONALE: If the maximal-protection apparatus vanished overnight, works from 1930 onward would enter the public domain on a rolling schedule, licensing markets would reprice around genuinely scarce services, anti-circumvention liability would collapse and interoperability and preservation work would resume openly, and educational reuse would expand immediately — the entire secondary-use economy reorganizes around open access.
% FOUNDING_PROBLEM: The Statute of Anne / Progress Clause problem: how to give authors a sufficient temporary return to induce writing and publishing without permanently locking knowledge away — exclusive right as a temporary means to a learning end.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the economists' amicus brief in Eldred v. Ashcroft (Akerlof et al., 2002) attests that retrospective extension supplies no creation inducement; the Copyright Office's own orphan-works report (2006) documents mass bodies of work whose owners cannot even be located, evidence the inducement function no longer organizes the field; academic term studies (Pollock, Heald) reach converging conclusions. Rights-holder testimony disputes this, attesting the incentive remains live for new production — hence 'contested' rather than 'dead'.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.80 at interval end) because the arrangement's marginal operations — retrospective term extension, anti-circumvention liability, fair-use narrowing — transfer value from the public domain and secondary users to catalog owners while supplying negligible prospective inducement at the margin being extended. Suppression is high (0.76) and is authored as a raw structural property, unscaled: statutory damages, criminalized circumvention, takedown regimes, and treaty ratchets are what hold the arrangement in place; the engine separately scales extractiveness by directionality and scope. Theater ratio is moderate (0.34) and rising: the promoting-progress and artist-welfare ceremonies continue while an increasing share of activity defends catalog value, but substantial activity remains functional (at extracting), so the ratio stays well below piton range. Accessibility collapse is moderate (0.48): alternatives persist — the rolling public domain, living fair-use doctrine, open licensing — but each is chilled or litigated. Resistance is substantial (0.62): constitutional challenges, library-lender litigation, and the open-licensing movement meet the arrangement continuously and lose slowly rather than not at all. The three series run on one shared seven-point grid so every metric is authored at every examined time point. The trajectory is a ratchet rather than a cycle: extensions cluster just ahead of iconic expiries (the 1998 act preceded the first frozen-cohort expiry by five years), a timing pattern that functions as intermittent reinforcement — each crisis-window conversion resets opposition before it accumulates.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats compute a world of stable, legitimate property: from inside a studio or label the arrangement is simply ownership doing what ownership does, indefinitely. The payer seats compute enclosure: from a documentary editor's or an archivist's desk the same structure is a toll booth on materials whose makers are frequently unfindable or long dead. The administrator seats compute routine: legislators, judges, and trade negotiators experience settled law and established process while sitting on the capture side of the ledger (contributions, advisory access, docket economy). Same-level divergence is sharpest between libraries_archives_museums and the studio seats — both institutionally powerful, yet the libraries' identity-fusion with permanent access removes the exit the studios' portfolio mobility preserves, so identical nominal standing yields opposite experienced constraints. Coalition potential among the powerless targets is real but blunted by heterogeneous immediate interests (consumers want price, educators want exceptions, archivists want preservation carve-outs); the lender litigation era marks the first durable convergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidy end: catalog owners collect directly and hold arbitrage-grade exit, damping their effective burden toward zero or below. Declared victims sit near the full-target end, modulated by exit: trapped consumers and future generations take the fullest incidence; constrained creators and educators slightly less; identity-locked institutions take high incidence despite institutional power because mission fusion blocks exit. Administrator seats carry no declared beneficiary or victim position and fall to derived/fallback directionality near symmetric, with capture-side leanings noted qualitatively. No directionality overrides are authored: overrides key on power atoms, and this story's institutional seats deliberately diverge (arbitrage-capable beneficiaries versus captured-but-constrained administrators), so a blanket institutional override would cross-contaminate seats the structural derivation already distinguishes through declared roles and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The incentive-to-create rationale, taken alone, would read as genuine coordination — a rope-like guarantee solving the public-goods problem of funding expensive works. Declaring victims, active enforcement, and suppressed exits prevents that mislabel: at the operative margin the coordination cover does no work, and what remains is coercion-maintained transfer. The converse error is equally blocked: because a real prospective incentive survives for new production within existing terms, the arrangement is not an inertial husk kept alive by habit — the machinery actively collects, and the payer set is identifiable and growing. The founding problem is therefore authored contested rather than dead: the parties genuinely dispute whether the inducement function lives, and the mismatch machinery should read that dispute rather than a forced resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the copyright_constitutional_mandate kernel. Which reading of the grant governs — property-maximalist mandate, public-scaffold limit, or judicial-deference discretion — and how would a sibling reading restructure the arrangement?',
    'Doctrinal evolution: a direct term-length challenge reaching the Supreme Court with a changed composition, treaty renegotiation reopening term floors, or sustained legislative reversal of the extension pattern.',
    'If public_scaffold_reading gained doctrinal force, epsilon falls sharply and the arrangement migrates toward transitional-support classification with sunset pressure on terms; if judicial_ambiguity_reading consolidated, epsilon persists but authority relocates wholly to legislative discretion. This file''s high-epsilon structure holds only under the present reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the constitutional copyright kernel governs the standing arrangement.').

omega_variable(
    retrospective_incentive_efficacy,
    'Does extending protection on already-created works produce any measurable addition to new creation?',
    'Econometric term studies and cross-jurisdiction natural experiments comparing creation and publication rates around term changes, updating the Eldred-era economists'' findings with streaming-era data.',
    'Near-zero marginal incentive collapses the coordination cover at the operative margin and consolidates the extractive classification; a demonstrably positive effect would restore a genuine coordination component and move the arrangement toward hybrid territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retrospective_incentive_efficacy, empirical, 'Whether retrospective term extension retains any incentive function.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (statutory damages, anti-circumvention liability, treaty locks) or internalized (a permission culture in which creators abandon otherwise lawful uses before any threat arrives)?',
    'Post-reform trajectory: if jurisdictions that broaden exceptions or shorten terms see lawful reuse rise slowly and self-censorship persist for years, a large internalized share is indicated.',
    'If substantially internalized, effective suppression exceeds the statutory measure and would survive formal reform; classification consequences would lag legal change by a generation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized components of the permission culture.').

omega_variable(
    expiry_timed_extension_recurrence,
    'When the next block of iconic works approaches expiry, will the extension cycle repeat — and is there any principled terminus in this reading short of perpetuity by installments?',
    'Observe legislative behavior across the late-2020s and 2030s expiry windows; absence of any proposed stopping rule across successive cycles indicates an unbounded ratchet.',
    'Recurrence confirms the ''limited times'' qualifier constrains nothing, hardening the extractive classification and refuting the reading''s own limiting language; a demonstrated terminus would rehabilitate a bounded-property version of the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expiry_timed_extension_recurrence, empirical, 'Whether the extension ratchet is bounded by any principled stop.').

omega_variable(
    ai_training_front_displacement,
    'Will pending litigation over training generative models on copyrighted corpora redraw the fair-use boundary wholesale, displacing the current epsilon?',
    'Outcomes of the training-data cases working through the appellate courts, and any legislative settlement of the licensing-versus-exemption question.',
    'A broad licensing victory raises epsilon further and extends the payer set to model developers; a sweeping fair-use victory narrows the constraint''s reach and begins eroding the maximal-protection frame from its newest front.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_training_front_displacement, empirical, 'Whether the AI-training contest displaces the constraint''s boundary structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccm_cer_tr_t0, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(ccm_cer_tr_t0, observed).
narrative_ontology:measurement(ccm_cer_tr_t5, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(ccm_cer_tr_t5, observed).
narrative_ontology:measurement(ccm_cer_tr_t10, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(ccm_cer_tr_t10, observed).
narrative_ontology:measurement(ccm_cer_tr_t15, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(ccm_cer_tr_t15, observed).
narrative_ontology:measurement(ccm_cer_tr_t20, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(ccm_cer_tr_t20, observed).
narrative_ontology:measurement(ccm_cer_tr_t25, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement_basis(ccm_cer_tr_t25, observed).
narrative_ontology:measurement(ccm_cer_tr_t30, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(ccm_cer_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(ccm_cer_be_t0, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(ccm_cer_be_t0, observed).
narrative_ontology:measurement(ccm_cer_be_t5, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement_basis(ccm_cer_be_t5, observed).
narrative_ontology:measurement(ccm_cer_be_t10, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement_basis(ccm_cer_be_t10, observed).
narrative_ontology:measurement(ccm_cer_be_t15, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(ccm_cer_be_t15, observed).
narrative_ontology:measurement(ccm_cer_be_t20, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(ccm_cer_be_t20, observed).
narrative_ontology:measurement(ccm_cer_be_t25, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(ccm_cer_be_t25, observed).
narrative_ontology:measurement(ccm_cer_be_t30, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement_basis(ccm_cer_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(ccm_cer_su_t0, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(ccm_cer_su_t0, observed).
narrative_ontology:measurement(ccm_cer_su_t5, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(ccm_cer_su_t5, observed).
narrative_ontology:measurement(ccm_cer_su_t10, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(ccm_cer_su_t10, observed).
narrative_ontology:measurement(ccm_cer_su_t15, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(ccm_cer_su_t15, observed).
narrative_ontology:measurement(ccm_cer_su_t20, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(ccm_cer_su_t20, observed).
narrative_ontology:measurement(ccm_cer_su_t25, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement_basis(ccm_cer_su_t25, observed).
narrative_ontology:measurement(ccm_cer_su_t30, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(ccm_cer_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional copyright' decomposes into three structurally distinct readings of one fixed-text kernel; per the epsilon-invariance principle each is a separate constraint story with its own epsilon, beneficiaries, victims, and classification. This file instantiates the corporate_enclosure_reading (grant as property requiring maximal protection; high epsilon). The public_scaffold_reading shares the referent — the standing arrangement — but authors sharply lower epsilon because it reads the monopoly as instrumental to a public-domain end. The judicial_ambiguity_reading addresses the same arrangement's authority structure rather than its transfer structure. Edges: the deference doctrine consolidated under the judicial_ambiguity_reading is the enforcement substrate this reading rides (upstream influence on this file's persistence), while this reading's entrenchment creates downstream repudiation pressure on the public_scaffold_reading's remaining doctrinal footholds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
