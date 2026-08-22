% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV Exclusive Inspiration Doctrine (Exclusive-Inspiration Reading)
 *   domain: religious/textual_criticism/theological_authority
 *
 * SUMMARY:
 *   Within a transnational network of congregations, Bible colleges, and
 *   publishers, the doctrine that the King James Version alone is the
 *   inspired, inerrant English Bible functions as the admission criterion for
 *   teaching roles, the product line of an apologetics publishing economy,
 *   and the trigger for member discipline. Modern translations are declared
 *   corrupted or satanic; their translators are defamed; the leadership
 *   network holds sole custody of textual truth. The KJV text itself is
 *   public domain, so nothing here rides on copyright — the scarce good the
 *   arrangement controls is legitimacy, not the artifact. This file
 *   instantiates ONE reading of the kernel kjv_text_1611 (see
 *   kernel_context); the sibling readings are separate constraints with
 *   separate epsilon values and empty or different victim sets, linked via
 *   network.affects_constraints. Claim and metrics are authored
 *   independently: the claimed type reflects the judgment that a genuine
 *   shared-text coordination function coexists with enforced asymmetric
 *   extraction, while the metrics describe the arrangement's observed
 *   operation. KEY AGENTS (by structural relationship): -
 *   kjv_only_leadership: agenda-setter and primary collector
 *   (organized/identity_locked) — administers the exclusivity claim, accrues
 *   authority and revenue - kjv_only_publishers: secondary beneficiary
 *   (moderate/mobile) — sells the defense literature - kjv_only_congregants:
 *   primary target (powerless/identity_locked) — bear discipline, restricted
 *   access, and fused-exit costs - modern_translation_users: diffuse target
 *   (moderate/mobile) — their Bibles are declared illegitimate -
 *   textual_criticism_scholars: target (organized/mobile) — defamed as
 *   corrupters - silenced_dissenting_members: excluded voice
 *   (powerless/trapped) - non_kjv_evangelical_pastors: excluded voice
 *   (moderate/mobile) - religious_studies_researchers: analytical observer
 *   (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.74).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.78).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV Exclusive Inspiration Doctrine (Exclusive-Inspiration Reading)").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/textual_criticism/theological_authority").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, '04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4').
narrative_ontology:cs_kernel_codification('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', fixed_text).
narrative_ontology:cs_authority_grounding('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', lineage).
narrative_ontology:cs_interpretation_layer_present('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4').
narrative_ontology:cs_reading_relation('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_axiom('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', foundational, kjv_exclusive_verbal_inspiration).
narrative_ontology:cs_axiom_status(kjv_exclusive_verbal_inspiration, holdable).
narrative_ontology:cs_axiom_grounding('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', kjv_exclusive_verbal_inspiration, theological).
narrative_ontology:cs_axiom('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', secondary, critical_text_corruption_claim).
narrative_ontology:cs_axiom_status(critical_text_corruption_claim, holdable).
narrative_ontology:cs_axiom_grounding('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', critical_text_corruption_claim, empirically_contingent).
narrative_ontology:cs_reference_frame('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', perfect_preservation_1611).
narrative_ontology:cs_drift_state('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', post_papyri_and_cbgm_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('04c9d21c-cbf4-4a10-98eb-8692ed6ba7b4', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, kjv_only_congregants).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_users).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, textual_criticism_scholars).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, kjv_verbal_preservation_doctrine).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, textus_receptus_purity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pastors, evangelists, authors, and Bible-college presidents who decide which translation their institutions may use, preach the exclusivity claim from pulpits and conference platforms, and correct or discipline staff and members who read from other versions. Their published works, speaking calendars, ordination credentials, and donation bases are all built around defending the King James text; stepping back from the claim would mean repudiating decades of ministry output.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, beneficiary).

% Print and sell King James editions, study aids, and books alleging corruption in modern versions. Sales track the credibility of the exclusivity claim; if congregations accepted multiple translations, the specialized catalog would lose its buyer base. They could shift to other devotional products at modest cost, so their stake is commercial rather than existential.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishers, beneficiary,
    moderate, biographical, mobile, national).

% Attend, tithe, and raise children inside congregations where the King James text is presented as the only true English scripture. Members who bring a modern translation to class may be corrected publicly, barred from teaching, or visited by leadership; asking why is framed as doubting God's word. Leaving usually means leaving friends, family networks, and an entire interpretive community, and is read by those left behind as spiritual fall.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_congregants, payer,
    powerless, biographical, identity_locked, local).

% Christians in mainline and broader evangelical circles who read the NIV, ESV, CSB, and similar versions. They encounter the claim chiefly as reputational pressure — their Bibles described as corrupted or worse in literature and conversations they did not seek out — and as friction in mixed-family and ecumenical settings. Most simply keep reading; the burden is intermittent rather than structural.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_users, payer,
    moderate, biographical, mobile, global).

% Academic specialists who compare Greek and Hebrew manuscripts and produce the base texts behind modern versions. Movement literature attributes their work to conspiracy, occult sympathies, or fraud, and institutions employing them face donor and church pressure where the claim takes hold. Their standing outside the movement is secure, so the cost lands as reputational damage within overlapping religious publics rather than as vocational ruin.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, textual_criticism_scholars, payer,
    organized, generational, mobile, continental).

% Members who privately prefer or quietly read modern translations but never say so in business meetings, Sunday school, or pulpit-committee discussions. Their preference has no channel: voicing it invites correction or discipline, so it surfaces only in anonymous comments, private conversations, or after departure.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, silenced_dissenting_members, excluded,
    powerless, biographical, trapped, local).

% Pastors of churches using modern versions who lose members to King James-only congregations, appear as cautionary examples of compromise in movement literature, and find joint evangelistic projects complicated by the charge that their Bibles are defective. They are discussed in the movement's councils but never consulted by them.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, non_kjv_evangelical_pastors, excluded,
    moderate, biographical, mobile, national).

% Sociologists, historians, and textual scholars who document the movement's origins in early twentieth-century fundamentalist controversies, trace its publishing and conference economy, and compare its manuscript claims against the academic consensus. They neither pay nor collect under the arrangement and can describe the whole structure, including flows the participants do not discuss.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, religious_studies_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:fixing_cost_class(kjv_text_1611__exclusive_inspiration_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single fixed English text for worship, memorization, cross-generational continuity, and communal identity, solving the fragmentation problem that unconstrained translation choice could pose for a congregation.
% TRANSFER_FUNCTION: Moves authority, deference, and money (offerings justified by the defense effort, book-table and conference revenue) from congregations to the leadership network that adjudicates which text counts as true scripture; moves legitimacy away from translators and textual scholars.
% ABSENT_VOICES: Members who privately read modern translations, former members who left over the issue, and mainstream evangelical pastors using modern versions — none sit in the councils where the doctrine is formulated and promulgated; textual critics appear in the movement's literature only as objects of denunciation, never as interlocutors.
% DISAPPEARANCE_RATIONALE: If the exclusivity claim vanished overnight, King James-only congregations would begin reading from multiple translations within months, the specialized apologetics catalog would lose its buyer base, leadership authority premised on sole custody of the true word would collapse into ordinary pastoral authority, and the disciplinary machinery (public correction, teaching bans, member visits) would lose its object.
% FOUNDING_PROBLEM: The late nineteenth- and early twentieth-century revisions (English Revised Version 1881, American Standard 1901) replaced familiar verses and rested on older manuscripts than the Textus Receptus; conservatives committed to verbal inspiration experienced this as the ground moving beneath the English Bible. The doctrine was built to end that instability by declaring one fixed English text the untouched, inspired word of God, settling manuscript disputes by fiat.
% FOUNDING_PROBLEM_CORROBORATION: Academic histories of the King James Only controversy written outside the movement, the textual-criticism literature, and published accounts of former members attest that the founding alarm was a sincere conservative reaction to real manuscript upheaval whose specific factual premises the relevant expertise has since rejected; movement leadership attests the problem is live. No attestation of liveness comes from outside the benefiting parties.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74) because the gatekeeping function is decoupled from service delivered: the claim converts ordinary pastoral authority into custody of divine speech, and revenue streams (defense literature, conferences, offerings framed as contending for the faith) ride on the claim's maintenance. Suppression (0.78) is a raw structural property, unscaled by power or scope: enforcement is continuous — pulpit denunciation, church-covenant translation clauses, public correction, teaching bans — precisely because nothing else holds the arrangement up; the KJV text is freely available to everyone, so only active delegitimation of alternatives sustains exclusivity. Theater ratio (0.40) reflects decay from the movement's early phase, when Burgon-style manuscript argumentation was at least live scholarship, toward recycled charts, accusation catalogs, and anniversary rhetoric; real enforcement keeps it well below inertial-performance levels. Accessibility collapse (0.50): alternatives remain physically ubiquitous (any bookstore) but collapse socially inside governed communities — removed from pews, classrooms, and permissible conversation. Resistance (0.60): steady scholarly rebuttal, member attrition, and broad evangelical rejection meet the claim, yet it persists. The temporal series run on one shared grid (points 0-60 by 10) with end-state values matching the scalar properties. The suppression_requirement series is authored deliberately: the story specifically tracks enforcement-capacity buildup (covenant adoption, formalized discipline, college accreditation fights), producing a monotonic ratchet rather than a cycle — no oscillation mechanism is present, so no cyclical measurement extension applies. Coalition note: the payer seats are numerous but dispersed across autonomous congregations and bound by identity fusion, which has so far prevented any durable payer coalition; the analysis treats coalition potential as latent, not realized.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat the arrangement is experienced as stewardship — contending for the faith against corruption — and computed from that seat it presents as protective coordination. From the congregant seat the same structure operates as surveillance of reading habits and punishment of questions. From the scholar seat it is organized defamation. Identity-lock binds differently at each seat: for leadership it is professional identity (published works, ministries, credentials constituting the self); for congregants it is ideological-relational fusion (doubting the text equals doubting God and estranging family). If either identity frame broke — a leader recanting, a congregation reframing the KJV as treasured heritage rather than exclusive revelation — the seats would converge rapidly, because no material infrastructure enforces the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership sits near the beneficiary pole: the claim subsidizes their authority and income, and their identity-lock amplifies rather than dampens the subsidy. Publishers share the beneficiary side with weaker pull (commercial stake, mobile exit). Congregants sit near the full-target end: they fund, comply, and cannot exit without identity rupture, and identity-lock pushes them toward maximal effective extraction. Modern translation users are nominal targets whose mobile exit and limited exposure place them mid-high rather than maximal. Scholars are targeted reputationally but vocationally secure — mid-range. Silenced dissenting members carry the highest directional load among payers: trapped inside the enforcement perimeter with no voice. Spatial scope is global for the claim but local for the costs; verifying 'corruption' allegations is hardest at the scope where the claim circulates, which amplifies effective extraction on the trapped seats while leaving mobile seats largely unaffected.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had a real coordination core: a community destabilized by manuscript upheaval needed a fixed shared text, and one text solved fragmentation, memorization continuity, and liturgical unity. The exclusivity claim converted that coordination into custody. Reading the arrangement as pure extraction would erase the genuine shared-text function members sincerely value; reading it as pure coordination would erase the discipline machinery, the defamed scholars, and the capture of gains by the agenda-setting seat. The tangled-rope classification preserves both halves. On the genealogy interview, founding-problem status is contested and the disappearance verdict is world_rearranges — the mismatch flag marks an arrangement persisting past its evidentiary warrant without asserting universal bad faith among its holders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_split,
    'This constraint is one reading of the kernel kjv_text_1611 (the exclusive_inspiration_reading). Would instantiating a sibling reading — revisable_translation_reading or functional_equivalence_reading — relocate the entire victim set, and where exactly does the disagreement sit?',
    'Compile and compare the sibling stories: the sibling readings authorize plural translation use, so their victim sets empty out and their extraction collapses toward coordination cost; the disagreement is located in the axiom that inspiration attaches exclusively to one English text.',
    'If a sibling reading is the better structural fit, this story''s victims (disciplined congregants, defamed scholars) vanish and the arrangement reduces to a literary-preference norm; if this reading fits, the gatekeeping structure stands as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_split, conceptual, 'Committer-frame ambiguity: which reading of the KJV-authority kernel governs, and what turns on the choice.').

omega_variable(
    adherent_welfare_ambiguity,
    'Do rank-and-file adherents experience the single-text regime as protective and stabilizing rather than burdensome, such that part of the measured extraction is experienced from inside as benefit?',
    'Congregation-level survey and interview data on member wellbeing, giving patterns, and voluntary versus coerced retention, compared against matched congregations using multiple translations.',
    'If adherents'' reported welfare is genuinely higher, effective extraction concentrates almost entirely on dissenters and outsiders and the coordination component weighs heavier; if retention is fear-driven, the current profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adherent_welfare_ambiguity, empirical, 'Whether insider experience offsets the outsider-measured extraction.').

omega_variable(
    manuscript_evidence_separability,
    'Is the empirical question of which Greek text-type better preserves the autographs separable from the authority claim that one English translation is exclusively inspired?',
    'Evaluate the Byzantine-versus-Alexandrian question on internal textual-criticism evidence alone, bracketing ecclesial authority; observe whether movement adherence tracks the evidence or the authority structure.',
    'If separable, the arrangement''s persistence despite adverse expert consensus marks it as authority-maintaining rather than evidence-following, sharpening the extraction reading; if inseparable, part of the enforcement defends a sincerely held empirical thesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manuscript_evidence_separability, empirical, 'Entanglement of a contestable scholarly claim with an authority claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression keeping members inside the single-text regime structural (family networks, community ties, livelihoods tied to ministry schools) or internalized (belief fusion in which questioning the text equals doubting God)?',
    'Post-exit trajectory study: if former members report persistent guilt, scriptural anxiety, and inability to read other translations years after leaving, the internalized share is large; if costs fall away quickly on exit, the structural share dominates.',
    'If largely internalized, effective suppression exceeds the observable enforcement record and exit looks freer than it is; if largely structural, alternative communities and financial independence would lower suppression faster than doctrinal debate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in member retention.').

omega_variable(
    authority_revenue_capture_ratio,
    'Of the gains flowing to leadership, what proportion is authority and deference versus direct money (book tables, conference fees, offerings justified by the defense effort)?',
    'Financial disclosure from movement ministries and publisher sales data where available; otherwise infer from the budget composition of representative ministries.',
    'A money-dominant mix strengthens the capture reading of the agenda-setter seat; an authority-dominant mix indicates the primary collected good is status, changing what reform would have to buy out.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_revenue_capture_ratio, empirical, 'Composition of the leadership capture between money and authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(kjv__tr_t10, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(kjv__tr_t30, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(kjv__tr_t50, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 50, 0.37).
narrative_ontology:measurement(kjv__tr_t60, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(kjv__be_t10, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(kjv__be_t30, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(kjv__be_t50, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 50, 0.73).
narrative_ontology:measurement(kjv__be_t60, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 60, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(kjv__su_t10, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(kjv__su_t20, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(kjv__su_t30, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(kjv__su_t50, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 50, 0.77).
narrative_ontology:measurement(kjv__su_t60, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the authority of the KJV' covers three structurally distinct constraints (epsilon-invariance decomposition): the exclusivity regime (this file — high epsilon, victims include disciplined members and defamed scholars), the revisability stance (low epsilon, no victim set — a scholarly norm about improvable translations), and the functional-pluralism stance (near-zero epsilon — literary and clarifying valuations coexisting). The family's upstream element is the historical prestige of the 1611 translation; the exclusivity reading cites that prestige as evidence for exclusive inspiration, so prestige flows downstream into gatekeeping. Epsilon differs across the family because the referent arrangement differs, not because a single constraint was measured with different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
