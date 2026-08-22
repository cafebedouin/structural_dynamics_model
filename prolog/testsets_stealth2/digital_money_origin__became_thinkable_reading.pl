% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Conceivability-Threshold Dating of Digital Money's Origin
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   A historiographical convention fixes the origin of digital money at the
 *   moment the concept became technically and institutionally conceivable —
 *   public-key cryptography, electronic funds-transfer architectures, and
 *   early electronic-cash designs of the 1970s-1980s — rather than at first
 *   practical holding or at regulatory recognition. The convention is
 *   administered by disciplinary gatekeepers (journals, textbook authors,
 *   professional associations, museum curators) and routes attribution goods
 *   — citation priority, founder designation, commemorative attention,
 *   funding-lineage prestige — to the early institutional architects of the
 *   concept, while practitioners who argue for practice-based origins and
 *   implementers who built working systems outside the canonical lineage bear
 *   citation marginality and narrative exclusion. CONSTRAINT FAMILY: this
 *   story is one reading of the kernel digital_money_origin; the sibling
 *   constraints digital_money_origin_first_held_reading and
 *   digital_money_origin_regulatory_recognition_reading instantiate different
 *   origin criteria with different beneficiary/victim sets and different
 *   epsilon values; this reading sits upstream, since its dating supplies the
 *   background chronology the others contest. Per the epsilon-invariance
 *   principle, the contest among readings lives in the linked stories and in
 *   the omega variables, not inside this constraint. KEY AGENTS (by
 *   structural relationship): - early_institutional_architects: primary
 *   beneficiary (powerful/identity_locked) — collects founder status,
 *   citation priority, commemorative standing - disciplinary_gatekeepers:
 *   agenda_setter (institutional/constrained) — administers the canon that
 *   fixes the dating - sponsoring_research_institutions: secondary
 *   beneficiary (institutional/mobile) — converts early patronage into
 *   institutional foresight prestige - practitioner_origin_advocates: primary
 *   target (organized/constrained) — pays in citation marginality and review
 *   friction - uncredentialed_digital_currency_builders: primary target
 *   (powerless/constrained) — bears exclusion from the founding narrative -
 *   monetary_authorities: excluded party (institutional/mobile) — contests
 *   the dating from outside the scholarly canon - early_practical_adopters:
 *   excluded party (powerless/trapped) — the constituency a rival dating
 *   would center - innovation_studies_scholars: analytical observer
 *   (institutional/analytical) — studies the origin controversy without a
 *   stake in it
 *
 * KEY AGENTS:
 *   - early_institutional_architects: primary beneficiary (powerful/identity_locked) — collects founder status and citation priority
 *   - disciplinary_gatekeepers: agenda_setter (institutional/constrained) — administers the canon
 *   - sponsoring_research_institutions: secondary beneficiary (institutional/mobile) — converts patronage into foresight prestige
 *   - practitioner_origin_advocates: primary target (organized/constrained) — pays in citation marginality
 *   - uncredentialed_digital_currency_builders: primary target (powerless/constrained) — bears narrative exclusion
 *   - monetary_authorities: excluded party (institutional/mobile) — contests the dating from outside the canon
 *   - early_practical_adopters: excluded party (powerless/trapped) — diffuse, unrecorded, unrepresented
 *   - innovation_studies_scholars: analytical observer (institutional/analytical) — no stake in which dating wins
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.52).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.35).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Conceivability-Threshold Dating of Digital Money's Origin").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, '24ca02e7-76c6-49cd-81cb-1876ec1c4c95').
narrative_ontology:cs_kernel_codification('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', distributed).
narrative_ontology:cs_authority_grounding('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', distributed).
narrative_ontology:cs_reading_relation('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', digital_money_origin__digital_money_origin_first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', digital_money_origin__digital_money_origin_regulatory_recognition_reading, coexists_with).
narrative_ontology:cs_axiom('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', foundational, technological_emergence_occurs_at_conceivability).
narrative_ontology:cs_axiom_status(technological_emergence_occurs_at_conceivability, holdable).
narrative_ontology:cs_axiom_grounding('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', technological_emergence_occurs_at_conceivability, conventional).
narrative_ontology:cs_axiom('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', secondary, pre_implementation_barriers_shape_the_artifact).
narrative_ontology:cs_axiom_status(pre_implementation_barriers_shape_the_artifact, holdable).
narrative_ontology:cs_axiom_grounding('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', pre_implementation_barriers_shape_the_artifact, empirically_contingent).
narrative_ontology:cs_reference_frame('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', conceivability_threshold_origin).
narrative_ontology:cs_drift_state('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', contemporary_crypto_cbdc_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('24ca02e7-76c6-49cd-81cb-1876ec1c4c95', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, sponsoring_research_institutions).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, practitioner_origin_advocates).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, uncredentialed_digital_currency_builders).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, linear_model_of_innovation).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, conception_priority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cryptographers, monetary theorists, and systems designers whose papers, patents, and prototypes defined digital cash as a concept — blind-signature schemes, electronic funds-transfer architectures, early electronic-wallet designs. Anniversary volumes, citation lineages, and founder designations route professional standing to them. Their legacies are constituted by the conception-dating: abandoning it would mean dissolving the identity their careers built, so they defend the dating as epistemic justice rather than as interest.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_institutional_architects, beneficiary,
    powerful, generational, identity_locked, global).

% Journal editors, textbook authors, professional associations, and museum curators who decide which origin story enters curricula, syllabi, reference works, and exhibitions. They administer the canon day to day — assigning reviewers, approving curricula, commissioning commemorative histories. Revising the dating would require reworking accumulated teaching materials and spending accumulated authority, a cost far exceeding what any single gatekeeper bears personally.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, disciplinary_gatekeepers, agenda_setter,
    institutional, generational, constrained, global).

% Central banks, defense research agencies, and corporate laboratories that funded the conceptual work in the 1970s-80s. The conception-dating credits their early patronage as institutional foresight, feeding narratives that justify current research programs and budget lines. They hold many funded lineages at once and can shift attribution emphasis at low cost, so their benefit is broad but shallow compared with the architects'.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, sponsoring_research_institutions, beneficiary,
    institutional, generational, mobile, global).

% Historians of technology and economic sociologists who argue that origins lie in practice — ledgers that balanced, systems people actually used — rather than in conception. They pay in citation marginality, hostile reviews, and framing as naive when they contest the dating inside the same journals and associations that enforce it. Their professional advancement runs through the venues whose canon they dispute, which keeps their exit costly.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, practitioner_origin_advocates, payer,
    organized, biographical, constrained, global).

% Implementers outside academia — early electronic-money operators, community-currency coders, game-economy designers — whose working systems predate or bypass the canonical lineage yet appear in the founding narrative only as footnotes or cautionary tales. They lack the credentials, archives, and institutional channels through which canonical recognition is granted, and bear the exclusion directly in how their work is described by others.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, uncredentialed_digital_currency_builders, payer,
    powerless, biographical, constrained, global).

% Central banks and regulators whose institutional self-narrative dates digital money to formal recognition — statistical inclusion, licensing regimes, supervisory frameworks. The conception-dating casts them as latecomers reacting to a fait accompli. They contest this framing in their own publications and staff histories, which they control, but cannot dislodge the scholarly canon from where they stand; their objection is voiced elsewhere, not in the venues that fix the dating.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_authorities, excluded,
    institutional, generational, mobile, national).

% Merchants and users who first held and transacted with non-physical monetary instruments in pilot programs, closed trials, and gray-market systems. Diffuse, unorganized, and historically ephemeral, most left few records and no advocacy organizations. The question of when their holding counted as digital money is answered without them; they are the constituency a practice-based dating would center and a conception-based dating renders invisible.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_practical_adopters, excluded,
    powerless, immediate, trapped, global).

% Science-and-technology-studies and innovation-studies researchers who treat origin controversies themselves as objects of analysis. They document how priority disputes are constructed, how credit migrates upstream, and how dating conventions stabilize — including this one — while holding no stake in which dating prevails.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, innovation_studies_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:fixing_cost_class(digital_money_origin__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, datable origin point for digital money that lets economists, historians, engineers, and policymakers teach, cite, fund, and regulate against a common genealogy; conception-dating made the field's history tractable in decades when adoption records were scattered, informal, or deliberately obscured.
% TRANSFER_FUNCTION: Moves attribution goods — citation priority, founder designation, commemorative attention, funding-lineage prestige — from later practitioners, implementers, and rival-dating advocates to the early institutional architects of the concept.
% ABSENT_VOICES: Early practical adopters and uncredentialed builders had no seat when the canon formed: they were diffuse, unrecorded, and lacked channels into the venues that fixed the dating. Monetary authorities voiced their rival dating only in their own publications, never inside the scholarly venues where the canon was set. Practitioner-origin advocates were present but taxed — heard chiefly as dissenters rather than as co-authors of the genealogy.
% DISAPPEARANCE_RATIONALE: If the conceivability-dating vanished overnight, curricula, funding lineages, commemorative infrastructure, and priority disputes would reorganize around whichever rival dating won: founder designations would migrate to early implementers or to recognizing authorities, textbook chronologies would be rewritten, and the architects' accumulated standing would partially decant to other constituencies. Arrangements demonstrably depend on the convention, so the world rearranges.
% FOUNDING_PROBLEM: Mid-century monetary innovation outran its categories: electronic funds transfer and then public-key cryptography produced value forms with no accepted origin story, and historians, funders, and policymakers needed a datable beginning to organize research programs, curricula, and institutional memory.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: innovation-studies scholarship independently documents the recurring need for datable technological origins, and the rival-dating advocates attest the need for a shared genealogy even while contesting this particular dating — they propose alternative thresholds rather than denying the coordination problem. However, the specific conceivability-threshold dating itself is attested almost exclusively by the architects and gatekeepers who benefit from it; no disinterested source affirms that threshold over its rivals.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52 at interval end): the convention channels real goods — founder designations, citation lineages, anniversary commemoration, funding-narrative leverage — to the conceptual community, but the goods are reputational rather than material and the field's core work proceeds regardless. Suppression is low-moderate (0.35): rival datings are publishable and live (the sibling readings exist as active positions), but venue access, curriculum placement, and review norms tax dissent. Theater is low-moderate (0.25): anniversary volumes and founder panels are partly performative, but the shared-genealogy function is real. Accessibility_collapse is low (0.40): accepting the conceivability dating does not eliminate rival periodizations — they persist as coherent scholarly positions. Resistance is elevated (0.55): practice-based historians, economic sociologists, and crypto-native chroniclers actively contest the dating, and their contestation intensified in the internet era. Time mapping assumption: T=0 corresponds to the early 1980s consolidation of the conception narrative (blind-signature cash designs, retrospective treatment of funds-transfer experiments); T=40 corresponds to the mid-2020s crypto-and-CBDC origin wars. The measurement series run on one shared seven-point grid with all three metrics authored at every point. The small terminal dips in base_extractiveness and suppression_requirement (T=34 to T=40) reflect the open-access and wiki-era relaxation of gatekeeping: enforcement capacity eroded slightly even as accumulated rents held, which is why resistance rose while extraction plateaued. Suppression here is a raw structural property of the convention (canon control, venue access), unscaled by power or scope; only extractiveness is scaled downstream.
 *
 * PERSPECTIVAL GAP:
 *   From the early_institutional_architects seat, the dating is simple epistemic justice: ideas precede artifacts, and credit belongs where the idea formed; their identity_locked exit means this is not a position they can trade away without dissolving their professional selves. From the payer seats, the same convention operates as enclosure of a collective achievement — working systems, user practices, and regulatory craft that made digital money real are recast as belated implementations of someone else's insight. From the disciplinary_gatekeepers seat, the convention is neutral stewardship of an inherited canon whose revision costs exceed any single editor's stake. From the monetary_authorities seat, the dating misstates history by undervaluing institutional work, casting regulators as reactive latecomers to a fait accompli. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The two declared beneficiary groups sit near the beneficiary end of directionality: early_institutional_architects receive the convention's transfers directly and cannot exit without identity loss (identity_locked stabilizes their subsidized position); sponsoring_research_institutions collect prestige indirectly and hold mobile exit, damping their effective benefit. The two declared victim groups sit near the target end: practitioner_origin_advocates pay in marginality inside the very venues that enforce the canon (constrained exit keeps their d high), and uncredentialed_digital_currency_builders bear outright narrative exclusion with no channel into the canon (powerless, constrained). The excluded seats — monetary_authorities and early_practical_adopters — hold no transfer relationship to this constraint's own flows; their grievance is representational, and the derivation correctly assigns them weak structural coupling. No directionality overrides were needed: beneficiary/victim declarations plus exit options reproduce the intended directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the need for a datable origin to organize research programs, curricula, and policy memory once electronic value transfer outran available categories — remains live: every new payment wave (stored value, mobile money, cryptocurrency, CBDC) reopens origin questions, so founding_problem_status is live and no mandatrophy is declared. The classification prevents two opposite mislabelings. Without the beneficiary/victim declarations, the convention presents as neutral scholarly consensus — a rope-like shared reference point that merely organizes the field. Without the coordination function, it presents as mere reputation-grabbing — a snare-like priority cartel. The tangled_rope claim holds both faces: a genuine genealogy-coordination function AND asymmetric attribution extraction sustained by active disciplinary enforcement. The receipt surface confirms the asymmetry: gains demonstrably accrue to a named seat (the architects), and fixing the convention is prohibitive relative to any single actor's stake, since the accumulated canon — decades of textbooks, curricula, exhibits, and funding lineages — would require wholesale revision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which criterion defines digital money''s ''emergence'' — technical-institutional conceivability (this reading), first practical holding of non-physical instruments, or formal regulatory recognition?',
    'Explicit stipulation of an emergence criterion by the consuming analysis, or meta-analytic comparison of the three linked sibling stories'' classifications under each candidate criterion.',
    'Switching criteria reassigns every beneficiary and victim, re-dates the constraint set by decades, and can move the classification between tangled_rope, rope, and scaffold-shaped profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'This constraint is one reading of the digital_money_origin kernel; the choice among readings is irreducible from inside any single story.').

omega_variable(
    thinkability_threshold_location,
    'When and where did digital money become technically and institutionally conceivable — public-key cryptography (mid-1970s), blind-signature electronic cash designs (early 1980s), or earlier electronic funds-transfer conceptions (1960s-70s)?',
    'Archival reconstruction of the earliest complete conception: a documented design in which non-physical value transfer was simultaneously technically specified and institutionally describable.',
    'Each candidate threshold shifts the origin date, changes which architects count as founders, and moves the measured start of the extraction series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_threshold_location, empirical, 'Location of the conceivability threshold inside this reading''s own frame.').

omega_variable(
    priority_rent_magnitude,
    'How much of the early institutional architects'' current standing derives from the dating convention itself rather than from the independent value of their technical contributions?',
    'Counterfactual citation and funding analysis comparing the architects'' accumulated standing against matched contributors under alternative periodizations.',
    'A large rent share supports the tangled_rope profile; a negligible share would collapse the extraction component toward a plain coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priority_rent_magnitude, empirical, 'Magnitude of attribution rents flowing through the dating convention.').

omega_variable(
    barrier_necessity_ambiguity,
    'Were the conceptual and regulatory barriers that kept digital money unimplemented before the 1990s natural limits of the technology, or constructed exclusions (bans on non-bank issuance, banking-charter boundaries) that the conception-dating retroactively naturalizes?',
    'Comparative jurisdictional history: implementation timing in jurisdictions without the barriers, and the sequence of adoption where barriers fell.',
    'If the barriers were constructed, the constraint set this reading dates includes deliberate exclusion, raising effective extraction and pulling the profile toward snare territory; if natural, the dating convention''s extraction stands alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_necessity_ambiguity, conceptual, 'Whether the pre-implementation barrier set was a natural limit or a constructed exclusion.').

omega_variable(
    suppression_mechanism_split,
    'Is the suppression of rival origin-datings structural (venue access, canon control, review gatekeeping) or internalized (scholars treating practice-based dating as self-evidently unscholarly)?',
    'Post-liberalization trajectory: if rival datings proliferate where open-access venues and wiki-scale reference works remove gatekeeping, residual reluctance is internalized.',
    'Internalized suppression persists after structural reform, keeping measured suppression above the structural baseline and sustaining extraction without active enforcers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural vs internalized split of the measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__became_thinkable_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(digi_tr_t7, digital_money_origin__became_thinkable_reading, theater_ratio, 7, 0.15).
narrative_ontology:measurement(digi_tr_t14, digital_money_origin__became_thinkable_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(digi_tr_t21, digital_money_origin__became_thinkable_reading, theater_ratio, 21, 0.21).
narrative_ontology:measurement(digi_tr_t28, digital_money_origin__became_thinkable_reading, theater_ratio, 28, 0.23).
narrative_ontology:measurement(digi_tr_t34, digital_money_origin__became_thinkable_reading, theater_ratio, 34, 0.25).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__became_thinkable_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__became_thinkable_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(digi_be_t7, digital_money_origin__became_thinkable_reading, base_extractiveness, 7, 0.41).
narrative_ontology:measurement(digi_be_t14, digital_money_origin__became_thinkable_reading, base_extractiveness, 14, 0.46).
narrative_ontology:measurement(digi_be_t21, digital_money_origin__became_thinkable_reading, base_extractiveness, 21, 0.5).
narrative_ontology:measurement(digi_be_t28, digital_money_origin__became_thinkable_reading, base_extractiveness, 28, 0.52).
narrative_ontology:measurement(digi_be_t34, digital_money_origin__became_thinkable_reading, base_extractiveness, 34, 0.53).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__became_thinkable_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__became_thinkable_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(digi_su_t7, digital_money_origin__became_thinkable_reading, suppression_requirement, 7, 0.26).
narrative_ontology:measurement(digi_su_t14, digital_money_origin__became_thinkable_reading, suppression_requirement, 14, 0.3).
narrative_ontology:measurement(digi_su_t21, digital_money_origin__became_thinkable_reading, suppression_requirement, 21, 0.33).
narrative_ontology:measurement(digi_su_t28, digital_money_origin__became_thinkable_reading, suppression_requirement, 28, 0.35).
narrative_ontology:measurement(digi_su_t34, digital_money_origin__became_thinkable_reading, suppression_requirement, 34, 0.37).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__became_thinkable_reading, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin_first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin_regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when did digital money emerge' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints: the conceivability-threshold dating (this story), the first-practical-holding dating, and the regulatory-recognition dating. Each has its own epsilon, its own beneficiary/victim structure, and its own classification; forcing one story to span all three would make epsilon observer-dependent. This reading is upstream in the family: its dating supplies the background chronology that the other two readings contest, so its influence edges point at both siblings. Neither edge forecloses: the disagreement is over the criterion for 'emergence,' a definitional dispute on which different parties can hold different positions without logical contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
