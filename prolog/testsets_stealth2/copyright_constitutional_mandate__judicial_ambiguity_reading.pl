% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Judicial Deference Zone for Copyright Term Length (Rational Basis Review)
 *   domain: legal/constitutional/political_economy
 *
 * SUMMARY:
 *   United States courts review congressional copyright term extensions only
 *   for a rational basis: since Eldred v. Ashcroft (2003) upheld the 1998
 *   Sonny Bono Act's twenty-year across-the-board extension — including its
 *   retroactive application to subsisting copyrights — the judiciary has
 *   treated 'limited Times' as a qualitative boundary whose quantitative
 *   content belongs to Congress alone. This file instantiates the
 *   judicial_ambiguity_reading of the copyright_constitutional_mandate
 *   kernel: the reading holds the phrase genuinely under-determines term
 *   length and that assigning the remainder to the elected branch is the
 *   constitutionally faithful allocation. The epsilon referent is the
 *   standing deference arrangement itself, assessed by this reading's own
 *   lights — not the enclosure regime the arrangement shelters and not the
 *   public-scaffold regime the Clause's preamble announces. The reading
 *   prices its own doctrine candidly: the arrangement performs a real
 *   separation-of-powers coordination function while measurably enabling a
 *   one-way term ratchet whose gains concentrate in incumbent rights-holding
 *   industries and whose costs fall on the diffuse public. Claim and metrics
 *   are authored independently: claimed_type records the structure this
 *   reading believes true (coordination plus asymmetric extraction plus
 *   active enforcement); the metrics record what the arrangement's 1998-2022
 *   operation descriptively shows. Interval units are years since the 1998
 *   Act (t0 = 1998, t24 = 2022).
 *
 * KEY AGENTS:
 *   - - us_supreme_court: Agenda setter (institutional/constrained) — authors and maintains the deference posture; collects institutional quiet
 *   - - congress_legislators: Primary beneficiary (institutional/arbitrage) — discretion vindicated, insulated from judicial check
 *   - - copyright_industry_rights_holders: Material beneficiary (organized/arbitrage) — extensions upheld; collects the monetizable gains
 *   - - general_public: Primary target (powerless/trapped) — bears extended terms diffusely
 *   - - derivative_creators_archivists: Secondary target (moderate/constrained) — clearance burdens, orphan-work paralysis
 *   - - future_generations_of_creators: Excluded seat — inherits the costs, holds no present voice
 *   - - public_interest_constitutional_litigators: Resisting payer/observer (organized/constrained) — litigated the challenge, lost, sustains the intellectual case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.44).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.5).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Deference Zone for Copyright Term Length (Rational Basis Review)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "legal/constitutional/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '64c7f07a-1215-487f-afa8-69ae92eb7e99').
narrative_ontology:cs_kernel_codification('64c7f07a-1215-487f-afa8-69ae92eb7e99', fixed_text).
narrative_ontology:cs_authority_grounding('64c7f07a-1215-487f-afa8-69ae92eb7e99', lineage).
narrative_ontology:cs_interpretation_layer_present('64c7f07a-1215-487f-afa8-69ae92eb7e99').
narrative_ontology:cs_reading_relation('64c7f07a-1215-487f-afa8-69ae92eb7e99', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_reading_relation('64c7f07a-1215-487f-afa8-69ae92eb7e99', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_axiom('64c7f07a-1215-487f-afa8-69ae92eb7e99', foundational, limited_times_textually_indeterminate).
narrative_ontology:cs_axiom_status(limited_times_textually_indeterminate, holdable).
narrative_ontology:cs_axiom_grounding('64c7f07a-1215-487f-afa8-69ae92eb7e99', limited_times_textually_indeterminate, empirically_contingent).
narrative_ontology:cs_axiom('64c7f07a-1215-487f-afa8-69ae92eb7e99', secondary, term_quantity_delegated_to_elected_branch).
narrative_ontology:cs_axiom_status(term_quantity_delegated_to_elected_branch, holdable).
narrative_ontology:cs_axiom_grounding('64c7f07a-1215-487f-afa8-69ae92eb7e99', term_quantity_delegated_to_elected_branch, conventional).
narrative_ontology:cs_reference_frame('64c7f07a-1215-487f-afa8-69ae92eb7e99', limited_times_as_legislative_delegation).
narrative_ontology:cs_drift_state('64c7f07a-1215-487f-afa8-69ae92eb7e99', post_eldred_serial_extension_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('64c7f07a-1215-487f-afa8-69ae92eb7e99', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congress_legislators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_industry_rights_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, general_public).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, derivative_creators_archivists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_interest_constitutional_litigators).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, rational_basis_deference).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, separation_of_powers_competence_allocation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored the governing posture in Eldred v. Ashcroft (2003): term extensions are reviewed only for a rational basis, with 'limited Times' read as a qualitative boundary and quantity left to Congress. Maintains the posture against recurring challenges; revisiting it would mean overturning recent precedent and drawing quantitative lines the majority has said courts lack competence to draw. What flows to the Court is institutional quiet — each deferential ruling avoids a confrontation with Congress that neither branch could cleanly win.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, us_supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Set term lengths by ordinary statute (currently life of the author plus seventy years; ninety-five years for corporate works) and know from Eldred that no court will second-guess the figure. The discretion is politically valuable: extensions are low-cost, high-gratitude favors to concentrated supporters, and the doctrine insulates those choices from judicial review. Nothing in the arrangement constrains them — they can lengthen, shorten, or decline to extend at will, and the arrangement's continued operation depends on their choosing not to.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congress_legislators, beneficiary,
    institutional, biographical, arbitrage, national).

% Studios, publishers, record labels, music catalogs, and authors' estates whose backlists gained roughly twenty additional years of exclusive revenue from the 1998 Act — including works already in existence, a windfall with no new creation attached. They lobby for extensions, fund the campaigns that sustain them, and coordinate internationally through treaty harmonization. Exposure to the arrangement is one-directional: it delivers terms, it never shortens them.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_industry_rights_holders, beneficiary,
    organized, generational, arbitrage, global).

% Readers, viewers, teachers, and local libraries who wait decades longer for works to enter the public domain. They cannot opt out of copyright terms, cannot sue to shorten them — standing doctrine and the deference posture close the courthouse path — and are too diffuse for any single member to feel the cost sharply enough to organize around it directly.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, general_public, payer,
    powerless, generational, trapped, national).

% Documentarians, oral historians, remix artists, and digital-preservation projects whose work requires clearing rights in older works. Long terms combined with orphan-work uncertainty make much of the twentieth-century catalog unusable in practice; their exits are partial — license when an owner can be located, redesign the project, or abandon it — but no exit removes them from the term regime itself.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, derivative_creators_archivists, payer,
    moderate, biographical, constrained, national).

% Not yet present: the artists, scholars, and readers of coming decades whose cultural inheritance is being fixed by today's extensions. They hold no standing, no vote, and no representative in the current bargain; every year added now is a cost assigned to them without their consent or participation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, future_generations_of_creators, excluded,
    powerless, civilizational, trapped, national).

% Public-interest lawyers, constitutional scholars, and library associations who challenged the 1998 extension to the Supreme Court and lost. They bear the litigation costs of testing the doctrine, supply the arguments that lower courts must now reject as foreclosed, and keep the intellectual case alive through journals and amicus briefs after the courtroom door closed.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_interest_constitutional_litigators, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, public_interest_constitutional_litigators, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_industry_rights_holders).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__judicial_ambiguity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates interpretive authority over an open-textured constitutional phrase: rather than courts inventing term numbers, the elected branch sets term policy under a predictable, uniformly applied minimal-scrutiny standard. It also solves a docket-management and institutional-peace problem — every term-length challenge receives the same answer, and neither branch is forced into a confrontation neither can cleanly win.
% TRANSFER_FUNCTION: Moves constitutional decision-making authority over term quantity from the courts-and-Congress dyad to Congress alone; downstream and mediately, it moves years of exclusive exploitation of existing works from the public domain to incumbent rights holders each time an extension passes — the doctrine moves authority, the statutes it shelters move the value.
% ABSENT_VOICES: Future creators and readers — the people whose cultural inheritance each extension diminishes — have no seat: no standing before an injury that has not yet matured, no vote, no lobby. Unorganized individual users are similarly voiceless; their interests appear only vicariously, through library and archive associations and through litigators who already lost their day in court. Dissenting justices spoke inside the room and were outvoted; the structurally absent are the unborn beneficiaries of the public domain.
% DISAPPEARANCE_RATIONALE: If the deference posture vanished overnight and courts applied meaningful scrutiny to term lengths, pending and future extensions would carry invalidation risk, Congress would begin drafting self-limiting terms to survive review, incumbents would reprice backlist catalogs against the possibility of earlier public-domain entry, and the one-way ratchet would stop. The arrangement's beneficiaries have built planning horizons on its reliability; its removal rearranges all of them.
% FOUNDING_PROBLEM: The Progress Clause commands exclusive rights for 'limited Times' but specifies no number, and the early Republic needed a workable allocation of that quantitative judgment; two centuries later, the modern Court needed a manageable standard for reviewing economic and social legislation generally. The arrangement was built to solve: who decides an open-ended policy quantity when the constitutional text fixes a quality ('limited') but not an amount.
% FOUNDING_PROBLEM_CORROBORATION: No one outside the benefiting parties attests that the problem is resolved in favor of practically unlimited discretion. Corroboration that the founding problem is real comes from across the spectrum: the drafting-era record (the Framers' own fourteen-plus-fourteen-year statute) attests that a quantity norm existed to allocate; the Eldred dissents (Stevens, Breyer) attest from inside the judiciary but outside the benefiting coalition that the limiting question remains live; and constitutional scholarship favorable and hostile to deference alike treats the allocation problem as genuine while disputing whether serial never-shortening extensions still constitute its faithful execution. Congress and rights-holder industries attest liveness only in the form that suits their continuation — their attestation is precisely the self-serving genealogy the corroboration rule exists to flag.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.44) because the doctrine extracts indirectly: it executes no transfer itself, but it is the load-bearing condition under which every extension since 1976 has survived challenge, and the 1998 extension's retroactive windfall (twenty added years on already-existing works, with no new incentive supplied) is the clearest single measure of what the arrangement yields to incumbents. Suppression (0.50) is a raw structural property, unscaled by power or scope: after Eldred the judicial-check alternative is closed to litigants as a class — subsequent challenges (Kahle v. Gonzales) were dismissed on the same posture — while non-litigious alternatives (Congress shortening terms, treaty differentiation, open licensing) remain formally open, which is why suppression sits mid-range rather than high. Theater_ratio (0.35) reflects a review that is partly performed: the Eldred majority surveyed two centuries of term history and textual argument, but the operative decision rule was deference, and post-Eldred dispositions increasingly resolve term disputes in a paragraph. Accessibility_collapse (0.58): heightened-scrutiny alternatives have largely collapsed for litigants but remain alive in scholarship and dissent, so collapse is substantial but incomplete. Resistance (0.62): sustained academic critique, two Supreme Court dissents, amicus coalitions, and continuing journalistic attention meet the doctrine continuously. The measurement series share one grid (t = 0,4,8,12,16,20,24) with every tracked metric authored at every point. The suppression_requirement series is deliberately non-monotonic: active defense of the arrangement peaked around the Eldred litigation and then decayed as precedent began doing the work passively — normalization of enforcement effort, not relaxation of the underlying closure, which the scalar suppression continues to register at 0.50. Receipt-surface note: gains demonstrably accrue to the rights-holder seat (retroactive term windfalls), and fixing is prohibitive for both potential fixers — the Court would spend precedent and institutional capital confronting Congress, and Congress would spend donor goodwill for a diffuse public benefit — so the receipt surface reads as captured-and-stuck even though this reading's structural claim remains tangled_rope on the strength of the genuine coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The three principal seats compute different constraints from the same doctrine. From the agenda-setter seat (the Court), the arrangement is institutional modesty functioning as designed: a manageable standard that keeps quantitative policy out of unelected hands and preserves inter-branch peace. From the beneficiary seats (Congress, rights-holder industries), it is reliable infrastructure: a known-safe channel for extensions, priced into every lobbying campaign since the 1962 interim extensions. From the payer seats (the public, derivative creators), it is the locked door: the reason the 1998 windfall was unchallengeable and the reason no future shortening can be judicially compelled. The powerless public seat rarely coalitions on this issue directly — library and archive organizations partially aggregate it, which is why the payer side shows organized amicus presence despite individually negligible stakes. The engine computes these per-seat classifications from the structural data; this reading's authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation: congress_legislators (beneficiary, arbitrage exit) derive d near the beneficiary pole — the arrangement subsidizes them with unconstrained discretion; copyright_industry_rights_holders (beneficiary, arbitrage) sit nearest the pole — the arrangement delivers and never takes from them; general_public (victim, trapped) derives near the full-target pole — no exit from the term regime and no judicial forum; derivative_creators_archivists (victim, constrained) sit slightly inside — partial exits via licensing and project redesign. A directionality override for the Court was considered — the canonical fallback for its power atom cannot see the Court's self-interested stake in restraint (each deferential ruling conserves judicial capital and avoids confrontation) — but the override schema keys on power_atom alone, and the story's other institutional agent (Congress) requires the opposite correction; a single institutional-keyed override would distort one seat to fix the other, so no override is authored and the asymmetry is documented here instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating judgment over an open-textured constitutional quantity — remains live in the abstract: some institution must set terms. What is contested is whether the arrangement still performs that allocation or has decayed into ratification. The mismatch consumer should watch founding_problem_status (contested) against disappearance_verdict (world_rearranges): the arrangement is load-bearing today, but if extensions become automatic — a ratchet no Congress ever reverses — the review component atrophies into ceremony and theater_ratio crossing 0.5 would mark proxy replacement, the classic mandatrophy signature. The tangled_rope classification prevents both mislabels: reading the arrangement as pure extraction erases the real separation-of-powers coordination that would survive even a perfectly public-regarding Congress; reading it as pure coordination erases the documented retroactive windfall transfers that no incentive rationale reaches. Holding both in one classification is what lets the corpus measure the drift between them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (judicial_ambiguity_reading) of the copyright_constitutional_mandate kernel; the sibling readings corporate_enclosure_reading and public_scaffold_reading instantiate different constraints with different epsilon values and victim structures. Which reading governs the arrangement in fact, and what would each sibling change structurally?',
    'Comparative classification across the linked constraint family; engine-computed foreclosure from axiom contradiction; observation of which reading the judiciary and Congress operationalize in the next major term-extension dispute.',
    'If corporate_enclosure_reading governs, the deference zone collapses into a maximal-protection regime and effective extraction rises sharply; if public_scaffold_reading governs, term limits acquire judicially enforceable content and this reading''s arrangement loses its subject matter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of three live readings of the copyright constitutional mandate kernel.').

omega_variable(
    limited_times_determinacy,
    'Is ''limited Times'' genuinely indeterminate — under-defining term quantity such that delegation is faithful interpretation — or determinate enough (via founding-era usage, the Framers'' own fourteen-plus-fourteen statute, and the Clause''s preamble purpose) that rational-basis deference amounts to abdication dressed as humility?',
    'Founding-era corpus linguistics, drafting and ratification history of the Progress Clause, and comparison with the term norms contemporaneous constitutional texts assumed.',
    'If determinacy is established, the arrangement''s coordination justification weakens toward vanishing, the extraction component dominates, and the composite shifts snare-ward; if indeterminacy is confirmed, the arrangement stands as a defensible allocation and epsilon stays low-to-moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limited_times_determinacy, empirical, 'Whether the textual ambiguity that licenses deference actually exists at the claimed depth.').

omega_variable(
    congressional_capture_share,
    'How much of Congress''s exercise of its deference-protected discretion is shaped by concentrated industry lobbying (campaign finance, revolving-door employment) relative to public-regarding judgment?',
    'Campaign-finance records correlated with term-extension sponsorship and roll-call behavior; comparison of extension bills'' origins against public-comment records.',
    'A high capture share converts the deference zone into a relay for private rents — effective extraction rises well above the authored base and the composite arrangement trends snare-ward; a low share supports the coordination-first reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_capture_share, empirical, 'Share of discretionary term-setting attributable to concentrated beneficiary lobbying.').

omega_variable(
    term_ratchet_perpetuity_trajectory,
    'Will the term ratchet continue one-way until terms are effectively perpetual (extension upon extension with no Congress ever shortening), or will terms stabilize at some equilibrium length?',
    'Observe whether any future Congress proposes or passes a term shortening, and whether extension bills become routine unanimous-consent items rather than contested legislation.',
    'Automatic extension marks the scaffold-to-enclosure transition completing through this reading''s gate — the review component atrophies (mandatrophy risk, theater_ratio climbing past 0.5); stabilization would mature the arrangement into ordinary legislative discretion and pull it rope-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_ratchet_perpetuity_trajectory, empirical, 'Trajectory of the term ratchet: de facto perpetuity versus stabilized discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(copy_tr_t0, observed).
narrative_ontology:measurement(copy_tr_t4, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement_basis(copy_tr_t4, observed).
narrative_ontology:measurement(copy_tr_t8, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(copy_tr_t8, observed).
narrative_ontology:measurement(copy_tr_t12, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement_basis(copy_tr_t12, observed).
narrative_ontology:measurement(copy_tr_t16, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(copy_tr_t16, observed).
narrative_ontology:measurement(copy_tr_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(copy_tr_t20, observed).
narrative_ontology:measurement(copy_tr_t24, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(copy_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 0, 0.33).
narrative_ontology:measurement_basis(copy_be_t0, observed).
narrative_ontology:measurement(copy_be_t4, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement_basis(copy_be_t4, observed).
narrative_ontology:measurement(copy_be_t8, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement_basis(copy_be_t8, observed).
narrative_ontology:measurement(copy_be_t12, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(copy_be_t12, observed).
narrative_ontology:measurement(copy_be_t16, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement_basis(copy_be_t16, observed).
narrative_ontology:measurement(copy_be_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(copy_be_t20, observed).
narrative_ontology:measurement(copy_be_t24, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement_basis(copy_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(copy_su_t0, observed).
narrative_ontology:measurement(copy_su_t4, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement_basis(copy_su_t4, observed).
narrative_ontology:measurement(copy_su_t8, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(copy_su_t8, observed).
narrative_ontology:measurement(copy_su_t12, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement_basis(copy_su_t12, observed).
narrative_ontology:measurement(copy_su_t16, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(copy_su_t16, observed).
narrative_ontology:measurement(copy_su_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement_basis(copy_su_t20, observed).
narrative_ontology:measurement(copy_su_t24, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 24, 0.35).
narrative_ontology:measurement_basis(copy_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, public_scaffold_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the constitutional copyright mandate' conflates three structurally distinct arrangements: (1) this file — the judicial deference zone that assigns term quantity to Congress (epsilon low-to-moderate; beneficiaries congressional authority and rights-holder industries; targets the diffuse public); (2) corporate_enclosure_reading — the maximal-protection property regime the deference zone shelters (substantially higher epsilon); (3) public_scaffold_reading — the public-domain-serving regime the Clause's preamble announces (low epsilon, coordination-forward). Each is a separate constraint with its own epsilon, beneficiaries, and victims, per the epsilon-invariance principle. They are linked because this reading sits upstream of both siblings as the doctrinal gatekeeper: it raises the enclosure regime's survival probability against constitutional challenge and lowers the scaffold regime's judicial enforceability, without logically eliminating either as a held position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
