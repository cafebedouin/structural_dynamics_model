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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Copyright as Maximal Property Estate: The Corporate Enclosure Reading of the Progress Clause
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   Under the corporate enclosure reading, the Progress Clause's promise of
 *   exclusive rights 'for limited Times' is read as a grant of near-absolute
 *   property in expression, where the only constitutional limit on term is
 *   the trivial one that a term must eventually end. Operatively, this
 *   reading has produced repeated term extensions (most recently twenty
 *   additional years in 1998, applied retroactively to existing works), a
 *   statutory anti-circumvention regime with criminal penalties, statutory
 *   damages scaled far beyond provable harm, and a fair-use doctrine narrowed
 *   case by case. The same body of law continues to finance and coordinate
 *   real creative production and licensing markets — that is its genuine
 *   coordination core — while the maximalist overlay transfers the
 *   overwhelming share of its marginal value to incumbent catalog holders
 *   whose works are already complete. KEY AGENTS (by structural
 *   relationship): hollywood_major_studios and major_record_labels: primary
 *   beneficiaries (powerful/arbitrage) — collect extended-term rents and
 *   purchase agenda-setting influence; congress_legislative_branch,
 *   us_copyright_office, federal_courts: agenda-setting seats
 *   (institutional/constrained) — enact, administer, and ratify;
 *   derivative_creators, educators, archivists_and_libraries: primary targets
 *   (powerless-to-organized/trapped-or-constrained) — bear liability,
 *   clearance costs, and locked collections; general_public: diffuse payer
 *   (powerless/trapped) — pays in delayed public domain and licensed-up
 *   prices; internet_platforms: dual-positioned intermediary
 *   (powerful/mobile) — safe-harbor beneficiary paying compliance costs;
 *   orphan_works_users: excluded seat (powerless/trapped) — silenced by
 *   after-the-fact liability; legal_academics: analytical observer
 *   (analytical/analytical) — documents the structure without a vote.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.78).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.74).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Maximal Property Estate: The Corporate Enclosure Reading of the Progress Clause").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, 'a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae').
narrative_ontology:cs_kernel_codification('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', fixed_text).
narrative_ontology:cs_authority_grounding('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', extraction).
narrative_ontology:cs_interpretation_layer_present('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae').
narrative_ontology:cs_reading_relation('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', foundational, copyright_is_natural_property_right).
narrative_ontology:cs_axiom_status(copyright_is_natural_property_right, holdable).
narrative_ontology:cs_axiom_grounding('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', copyright_is_natural_property_right, deontological).
narrative_ontology:cs_axiom('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', foundational, limited_times_permits_maximal_extension).
narrative_ontology:cs_axiom_status(limited_times_permits_maximal_extension, holdable).
narrative_ontology:cs_axiom_grounding('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', limited_times_permits_maximal_extension, conventional).
narrative_ontology:cs_axiom('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', secondary, maximal_protection_maximizes_creation).
narrative_ontology:cs_axiom_status(maximal_protection_maximizes_creation, holdable).
narrative_ontology:cs_axiom_grounding('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', maximal_protection_maximizes_creation, empirically_contingent).
narrative_ontology:cs_reference_frame('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', authorial_property_estate).
narrative_ontology:cs_drift_state('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', contemporary_post_ctea_doctrine, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('a874a9fb-f74c-4cd1-abbb-b9f13f17c8ae', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, hollywood_major_studios).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, major_record_labels).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists_and_libraries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, general_public).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, internet_platforms).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, general_public).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, internet_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold century-deep film catalogs whose characters and titles remain under exclusive control decades past their creators' deaths. Fund the trade associations that drafted and promoted term-extension and anti-circumvention legislation, and collect the resulting licensing, sequel, and merchandising revenue on works that would otherwise be public domain. Structure rights across jurisdictions to shop for the most protective regimes.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, hollywood_major_studios, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, hollywood_major_studios, agenda_setter).

% Control master recordings whose protection horizon has lengthened repeatedly during their corporate lifetimes. Through their trade association they supplied the drafting language for digital-era enforcement statutes and administer licensing pools that collect on both new releases and back catalogs. Revenue concentrates on catalog shares rather than on signing new artists.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, major_record_labels, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, major_record_labels, agenda_setter).

% Enacts the term extensions and enforcement statutes that operationalize the maximal-protection reading, most consequentially in 1998. Receives concentrated campaign contributions and drafting assistance from incumbent rightsholder industries while opposition arrives diffuse, late, and low-salience. Electoral timelines reward visible patronage over long-horizon public-domain accounting.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, congress_legislative_branch, agenda_setter,
    institutional, biographical, constrained, national).

% Administers registration, drafts the legislative recommendations that Congress routinely adopts, and runs the triennial process through which libraries and researchers must petition for narrow exemptions from anti-circumvention liability. Its institutional posture has consistently favored longer terms and stronger protection across administrations.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, us_copyright_office, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicate the scope of the exclusive rights, uphold term extensions under rational-basis review, and decide fair use case by case. Precedent binds successors; a single ratifying decision on term length forecloses challenge for a generation. Individual judges rotate, but the institutional memory of ratified maximalism persists.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Documentary filmmakers, remix artists, translators, and fan creators build on existing works, which exposes them to takedown demands and statutory damages far exceeding their means. Most cannot afford clearance or litigation, so they abandon projects or self-censor at the scripting stage. Switching to wholly original material means leaving the cultural conversation their form depends on.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    powerless, biographical, constrained, global).

% Universities and teachers need to excerpt, screen, digitize, and redistribute materials for instruction. Fair-use uncertainty and anti-circumvention liability push them toward expensive licensed course packs and away from primary sources. Their institutional weight secures occasional negotiated exemptions but not durable rights.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    organized, generational, constrained, national).

% Hold physical collections of 20th-century recordings, films, and manuscripts whose rightsholders are unidentifiable or defunct. Preservation copying and public access require permission that cannot be obtained, and anti-circumvention rules bar breaking digital locks even on deteriorating media they physically own. Custody of the material confers no usable rights; they must petition periodically for narrow exemptions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists_and_libraries, payer,
    organized, generational, trapped, national).

% Receives the stream of commercially produced works the system funds, and pays through extended exclusivity: delayed public-domain entry, licensing fees embedded in prices, and a thinner commons of freely reusable culture. Copyright is low-salience politics; no electoral penalty attaches to term extension, and no exit from a surrounding culture of owned expression exists.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, general_public, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, general_public, beneficiary).

% Operate user-generated content services under a bargain struck in the digital-era statutes: immunity from user-infringement liability in exchange for takedown infrastructure and filtering systems built at their own expense. Large platforms absorb the compliance cost and monetize the traffic; smaller entrants find the compliance floor a barrier. They retain the resources to relocate or restructure if any single regime turns hostile.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, internet_platforms, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, internet_platforms, payer).

% Publishers, museums, genealogists, and software maintainers who want to reprint, exhibit, or preserve works whose owners cannot be located. They are exposed to damages that arrive only after use, so the rational strategy is silence: the works simply go unused. They have no seat in legislative drafting and appear in policy debates only through the petitions of larger institutions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, orphan_works_users, excluded,
    powerless, biographical, trapped, global).

% Scholars of copyright and constitutional law document the divergence between the incentive rationale and the distribution of term-extension benefits, publish the empirical studies of production effects, and filed the constitutional challenge to the 1998 extension. Their analysis shapes amicus briefs and reform proposals but commands no vote and no veto.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, legal_academics, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__corporate_enclosure_reading, hollywood_major_studios).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__corporate_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines exclusive rights in expressive works through a uniform statutory framework, so that creative production can be financed through markets: licensing, distribution, credit, and cross-border trade in content are coordinated through a single rights vocabulary that everyone can transact in.
% TRANSFER_FUNCTION: Moves monopoly income on expressive works — extended-term licensing revenue, statutory-damages settlements, clearance fees, and control over derivative and preservation markets — from the general public, derivative creators, educators, and archives to incumbent catalog holders and their trade associations.
% ABSENT_VOICES: Future generations who would inherit the public-domain feedstock of the twentieth century; orphan-works users who cannot locate rightsholders and therefore use nothing; unrepresented audiences priced out of cultural access. They are absent because policy is made among incumbent holders, their trade associations, and the administering agencies; dissenting creators enter the process only as litigants after the rules are fixed.
% DISAPPEARANCE_RATIONALE: If the maximal-protection reading lost force overnight — terms reverting toward their historical lengths, anti-circumvention liability lapsing, fair use robustly protected — derivative markets would reopen, archives would begin mass digitization of twentieth-century holdings, platforms would shed filtering machinery, and incumbent back-catalog revenue would compress sharply. The creative economy would reorganize around a rapidly refilling public domain.
% FOUNDING_PROBLEM: Securing authors and their commercial publishers exclusive rights in copies of their works, so that investment in writing, printing, recording, and distribution could be recouped before competitors free-rode on the finished product.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent trade associations attest the problem is live, citing ongoing digital piracy. Outside the benefiting parties: empirical studies of term extension find no measurable increase in new production (extended terms attach overwhelmingly to already-existing works), government accountability reviews have attributed industry decline to business-model and technological factors rather than copying alone, and the constitutional challenge to the 1998 extension was argued on precisely this obsolescence by economists and historians. No fully disinterested attestation exists; the strongest external evidence supports the view that the original problem is solved for new works and was never the operative warrant for the extension layer.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.78) because the marginal operations of this reading — term extension on completed catalogs, anti-circumvention liability, fair-use narrowing — transfer value with little relation to any incentive the recipients still need; the 1998 step-change in the series marks the simultaneous arrival of the twenty-year extension and the anti-circumvention statute. Suppression (0.74) reflects the enforcement machinery the arrangement requires: statutory damages that dwarf actual harm, criminal provisions aimed at circumvention tools, takedown regimes that shift the cost of dispute onto speakers, and a triennial exemption process that converts rights back into privileges. Theater ratio (0.56, having crossed 0.5 around 2012) tracks the growing share of maximalist activity that performs the incentive rationale rather than serving it — extensions legislated for dead authors are the purest case — indicating Goodhart drift in which maximizing protection has replaced promoting progress as the operative goal. Accessibility collapse is moderate (0.52): genuine alternatives persist (the public domain, open licensing, surviving fair use) but each is hedged with litigation risk, which keeps them from functioning as real exits. Resistance (0.66) is substantial and organized: the constitutional challenge to the 1998 extension, the open-licensing movement, archive coalitions, and a sustained scholarly literature. The measurement series run on one shared eight-point grid (1976-2026) so every metric is authored at every examined time point; the trajectory is a ratchet rather than a cycle — recurring piracy panics (home taping, Napster, streaming) supply the crisis energy for discrete upward steps, followed by normalization, with no compensating downward phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical doctrine. From the studio seat the arrangement is earned property: investments made under one term-length expectation were retroactively extended, which the seat experiences as confiscation-prevention rather than gift. From the documentarian's seat the same doctrine is a liability minefield that censors at the scripting stage. From the congressional seat it is constituency service rendered to visible, grateful, contributing industries. From the bench it is settled text ratified under rational-basis review. The engine computes these divergent per-seat classifications from the structural data — power, exit, and directional position — and the divergence itself is the finding: a structure that is property to one seat and censorship to another is doing different things to different people through the same clauses.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: the studios and labels collect the marginal value of every extension and possess arbitrage-grade exit (global rights structuring), so effective extraction from them approaches zero or inverts. Targets sit near the full-target end: derivative creators (powerless, constrained), educators and archivists (organized but trapped by custody and mission), and the diffuse public (trapped inside an owned culture) bear the transfer with little offsetting benefit. The agenda-setting seats are the analytically delicate ones: Congress is structurally aligned with the beneficiaries through campaign finance despite appearing neutral, which pulls its derived directionality toward the beneficiary end; the courts behave nearer symmetric, ratifying whatever arrives. Platforms straddle: safe-harbor immunity subsidizes them while compliance costs tax them, netting to a mid-range position. The vindicated propositions — labor-desert and the property paradigm — collect no rents and are deliberately excluded from the beneficiary set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — induce authorship by securing recoverable investment — remains live for the base institution: new works still respond to exclusive-rights incentives, and the licensing vocabulary still coordinates real markets. But the mandate is dead for the maximalist overlay this reading drives: term extensions operate exclusively on works whose authors are dead and whose creation costs were recouped long ago, so the incentive rationale for the extension layer is performed, not functional. Reading founding_problem_status (contested) against disappearance_verdict (world_rearranges) surfaces the mismatch the corpus exists to catch: the arrangement persists and its removal would rearrange the world, yet the warrant offered for its marginal operations is disputed by everyone outside the benefiting parties. The classification discipline prevents two opposite errors: labeling all of copyright a snare (which erases the live coordination core that funds production) and labeling the maximalist apparatus a rope (which launders retroactive rent extension as incentive policy). The honest structure is a functioning coordination base carrying an increasingly extractive, increasingly theatrical overlay — a tangled rope drifting toward snare, with the drift visible in the rising extractiveness and theater series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates the corporate_enclosure_reading of the kernel copyright_constitutional_mandate; the sibling readings public_scaffold_reading and judicial_ambiguity_reading instantiate different constraints over the same Progress Clause text. Which reading governs any given legal outcome?',
    'Attribute each outcome to the seat whose commitments produced it: statutes authored through lobbied congressional processes instantiate the enclosure reading; doctrines authored by deferential courts instantiate the ambiguity reading; public-domain-expanding reforms instantiate the scaffold reading. Classify per producing seat, never pool.',
    'Epsilon, beneficiaries, and victims differ per reading: the scaffold reading relocates beneficiaries to the public and converts ''limited times'' into an operative sunset; the ambiguity reading strips the normative maximalism and leaves discretionary delegation. Pooling readings would fabricate a single epsilon for a multi-epsilon label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: one kernel, three readings, three distinct constraints.').

omega_variable(
    natural_property_vs_statutory_grant,
    'Is copyright a natural property right grounded in authorial labor-desert (from which maximal protection follows as a matter of justice), or a statutory instrument granted for public ends (in which limitation is constitutive rather than grudging)?',
    'Trace doctrinal treatment of the features that distinguish property from grant: termination rights, term reversion, the operative force of the Progress Clause preamble, and whether courts treat term length as entitlement or as policy lever.',
    'If the statutory-grant framing is correct, this reading''s foundational premise fails, ''limited times'' regains limiting force, and the arrangement recomputes as rent maintenance on a public instrument rather than protection of owned goods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_property_vs_statutory_grant, conceptual, 'Whether the property characterization that drives maximalism is descriptively or merely rhetorically true.').

omega_variable(
    term_extension_incentive_effect,
    'Does extending terms on already-existing catalogs measurably increase new creative production?',
    'Natural experiments: output of the post-2019 US public-domain cohorts versus the frozen 1923-1977 cohorts, cross-country comparison of production rates under different term lengths, and publisher behavior toward works approaching reversion.',
    'A null effect marks the extension layer as pure transfer to catalog holders and pushes the computed classification toward snare; a positive effect supports residual coordination function in the maximalist apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_extension_incentive_effect, empirical, 'Empirical warrant for the incentive rationale that legitimates term extension.').

omega_variable(
    suppression_structural_vs_chilled,
    'Is the measured suppression carried by enforcement machinery (statutory damages, anti-circumvention prosecutions, takedown volume) or by internalized caution (creators abandoning lawful uses without any demand ever being made)?',
    'Post-reform suppression trajectory: if creators continue avoiding uses after liability risk drops (works entering the public domain, new Library of Congress exemptions), the avoidance is internalized rather than structural.',
    'If internalized, effective suppression exceeds the structural measure and persists after legal reform; reform of the statute alone would not restore the suppressed activity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_chilled, empirical, 'Structural versus internalized suppression mechanism in the chilling of lawful reuse.').

omega_variable(
    legislative_capture_vs_constituency,
    'Does congressional maximalism reflect captured agenda-setting by incumbent-funded campaigns, or genuine constituent preference for strong intellectual property?',
    'Campaign-finance tracing around the 1998 term-extension and anti-circumvention votes contrasted with polling on term-length preferences; comparison of voting patterns across districts with and without incumbent rightsholder employers.',
    'Capture implies the prohibitive cost of fixing the arrangement is contingent on money buying agendas and could fall with campaign-finance reform; constituent preference implies deeper entrenchment independent of lobbying.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legislative_capture_vs_constituency, empirical, 'Source of the agenda-setter seat''s alignment with incumbent interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccm_corp_enc_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.25).
narrative_ontology:measurement(ccm_corp_enc_tr_t1985, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(ccm_corp_enc_tr_t1994, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1994, 0.35).
narrative_ontology:measurement(ccm_corp_enc_tr_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1998, 0.45).
narrative_ontology:measurement(ccm_corp_enc_tr_t2003, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2003, 0.48).
narrative_ontology:measurement(ccm_corp_enc_tr_t2012, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2012, 0.52).
narrative_ontology:measurement(ccm_corp_enc_tr_t2019, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2019, 0.54).
narrative_ontology:measurement(ccm_corp_enc_tr_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2026, 0.56).

% Extraction over time
narrative_ontology:measurement(ccm_corp_enc_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.48).
narrative_ontology:measurement(ccm_corp_enc_be_t1985, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(ccm_corp_enc_be_t1994, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1994, 0.58).
narrative_ontology:measurement(ccm_corp_enc_be_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(ccm_corp_enc_be_t2003, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2003, 0.71).
narrative_ontology:measurement(ccm_corp_enc_be_t2012, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2012, 0.75).
narrative_ontology:measurement(ccm_corp_enc_be_t2019, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2019, 0.77).
narrative_ontology:measurement(ccm_corp_enc_be_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ccm_corp_enc_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(ccm_corp_enc_su_t1985, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(ccm_corp_enc_su_t1994, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1994, 0.52).
narrative_ontology:measurement(ccm_corp_enc_su_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1998, 0.62).
narrative_ontology:measurement(ccm_corp_enc_su_t2003, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2003, 0.66).
narrative_ontology:measurement(ccm_corp_enc_su_t2012, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement(ccm_corp_enc_su_t2019, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2019, 0.73).
narrative_ontology:measurement(ccm_corp_enc_su_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2026, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the copyright constitutional mandate.' The single Progress Clause sentence covers three structurally distinct claims with distinct epsilons: this file authors the corporate_enclosure_reading (copyright as maximal property estate; corporate beneficiaries; derivative creators, educators, and archivists as victims; high epsilon). The public_scaffold_reading authors the same text as a transitional monopoly instrument whose beneficiaries are the public and whose 'limited times' clause is an operative sunset (low epsilon, scaffold-shaped). The judicial_ambiguity_reading authors the text as a zone of legislative discretion policed only by rational-basis review (epsilon indexed to whatever Congress enacts). The enclosure reading is downstream of the property-tradition lineage and upstream of enforcement practice: it supplies the substantive maximalism that deferential review ratifies. Linking all three via affects_constraints lets contamination analysis track, for example, how a scaffold-reading victory (public-domain expansion) erodes the enclosure reading's factual premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
