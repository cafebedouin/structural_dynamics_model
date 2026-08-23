% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954) — Constitutional Individual-Rights Reading
 *   domain: legal/constitutional/religious_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: the secular
 *   civil reading, in which marriage and family-law authority flows from a
 *   secular civil code (the Special Marriage Act 1954) grounded in
 *   constitutional individual rights and administered by civil courts. The ε
 *   authored here is for the standing civil-codal arrangement as the
 *   reading's own lights (individual-rights constitutionalism) assess it —
 *   NOT for the fully realized uniform code the reading would endorse, which
 *   would make ε trivially low by construction. Structurally, the arrangement
 *   solves a coordination problem no community regime can solve — marrying
 *   inter-religious couples without conversion, and providing symmetric
 *   divorce — while the same structure imposes its heaviest costs on its most
 *   vulnerable users through the 30-day public-notice machinery, with the
 *   systemic gains accruing to the state seat. The claim (tangled_rope) and
 *   the metrics are authored independently; the engine computes each seat's
 *   classification from the structural data.
 *
 * KEY AGENTS:
 *   - indian_state: agenda-setter and collector (institutional/arbitrage) — legislates the Act, runs the registration and notice machinery, collects the registry, legitimacy, and uniform-code template; can expand or shelve the track at will
 *   - civil_judiciary: agenda-setter (institutional/constrained) — adjudicates and interprets; recently holding the notice requirement dispensable in parts while continuing to operate it
 *   - interfaith_couples: primary beneficiary with payer costs (moderate/constrained) — the only non-conversion path to a valid marriage
 *   - notice_exposed_couples: primary target (powerless/trapped) — bear the public-notice window's exposure; often young, often women defying their families
 *   - community_exit_couples: secondary target (moderate/identity_locked) — pay standing community-exit costs after a cheap legal step
 *   - women_seeking_gender_equal_divorce: beneficiary (moderate/constrained) — symmetric divorce and succession inside the track
 *   - personal_law_communities: excluded (organized/mobile) — would contest the track's legitimacy claims; hold no seat in its design
 *   - civil_society_legal_aid: analytical observer (organized/analytical) — documents notice-window outcomes and litigates test cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.46).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.4).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954) — Constitutional Individual-Rights Reading").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "legal/constitutional/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '18920422-1b37-4ddb-ade9-362119bbc6c9').
narrative_ontology:cs_kernel_codification('18920422-1b37-4ddb-ade9-362119bbc6c9', formalized).
narrative_ontology:cs_authority_grounding('18920422-1b37-4ddb-ade9-362119bbc6c9', lineage).
narrative_ontology:cs_interpretation_layer_present('18920422-1b37-4ddb-ade9-362119bbc6c9').
narrative_ontology:cs_reading_relation('18920422-1b37-4ddb-ade9-362119bbc6c9', marriage_authority_kernel__hindu_codified_reading, influences).
narrative_ontology:cs_reading_relation('18920422-1b37-4ddb-ade9-362119bbc6c9', marriage_authority_kernel__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('18920422-1b37-4ddb-ade9-362119bbc6c9', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('18920422-1b37-4ddb-ade9-362119bbc6c9', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('18920422-1b37-4ddb-ade9-362119bbc6c9', foundational, individual_rights_supremacy_in_marriage).
narrative_ontology:cs_axiom_status(individual_rights_supremacy_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('18920422-1b37-4ddb-ade9-362119bbc6c9', individual_rights_supremacy_in_marriage, deontological).
narrative_ontology:cs_axiom('18920422-1b37-4ddb-ade9-362119bbc6c9', secondary, civil_registration_suffices_across_religions).
narrative_ontology:cs_axiom_status(civil_registration_suffices_across_religions, holdable).
narrative_ontology:cs_axiom_grounding('18920422-1b37-4ddb-ade9-362119bbc6c9', civil_registration_suffices_across_religions, conventional).
narrative_ontology:cs_reference_frame('18920422-1b37-4ddb-ade9-362119bbc6c9', constitutional_individual_rights_supremacy).
narrative_ontology:cs_drift_state('18920422-1b37-4ddb-ade9-362119bbc6c9', post_puttaswamy_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('18920422-1b37-4ddb-ade9-362119bbc6c9', '2026-08-04T09:15:00Z').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_gender_equal_divorce).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, indian_state).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, notice_exposed_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, community_exit_couples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, notice_exposed_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, community_exit_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, interfaith_couples).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, constitutional_individual_rights_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, secular_civil_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, uniform_civil_code_directive_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the Special Marriage Act 1954 and maintains the registration, public-notice, and court machinery that administers it. Collects a uniform civil registry, the constitutional legitimacy of offering a religion-neutral marriage path, and a working template cited in uniform-civil-code debates. It simultaneously maintains the parallel community-law regimes, so it can adjust, expand, or shelve the civil track at will.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, indian_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, indian_state, beneficiary).

% Adjudicates marriages, divorces, and succession for couples under the civil track, and interprets the Act's requirements. In recent years it has held the public-notice requirement dispensable in some circumstances, citing the constitutional right to privacy, while continuing to operate the registration and adjudication system day to day.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Couples from different religious communities who register under the civil Act because no community law will marry them unless one partner converts. They gain a legally valid, court-enforceable marriage; they pay the 30-day public notice, the paperwork, and often the community fallout. Before opting in, their alternative was conversion into one partner's community regime.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, interfaith_couples, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, interfaith_couples, payer).

% Women who choose the civil track for its symmetric divorce grounds, maintenance rules, and succession consequences, which do not vary with the husband's community. They exit the marriage through the same statutory court process as their spouses; some community regimes available to them instead offer fewer or slower exits.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_gender_equal_divorce, beneficiary,
    moderate, biographical, constrained, national).

% Couples — often young, and frequently including women who married against their families' wishes — whose intended marriage is published on a public notice board for 30 days before registration. During that window families and community members can locate them, pressure them, file objections, or worse; courts have documented harassment and violence following published notices. Most districts offer no way to skip the window, and once it begins, withdrawing or hiding carries its own costs.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, notice_exposed_couples, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, notice_exposed_couples, beneficiary).

% Couples who register civilly and in doing so step outside their community's marriage regime, forfeiting customary inheritance channels, community dispute-resolution, and social standing. The legal step is a signature; the standing cost is paid inside the community network they remain embedded in, and it follows them for years.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, community_exit_couples, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, community_exit_couples, beneficiary).

% Religious institutions and personal-law boards that administer marriage and divorce inside their communities. They lose no formal power under the civil Act — their regimes remain the default for most marriages — but each couple that registers civilly is a couple their institutions no longer adjudicate, and they publicly contest the civil track's claim to superior legitimacy and resist its expansion.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, personal_law_communities, excluded,
    organized, generational, mobile, national).

% Legal-aid clinics, women's rights organizations, and researchers who document what happens to couples during the notice window, litigate test cases, and publish case records. They hold no decision power over the Act's design but produce most of the evidence the judiciary and Parliament read.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_society_legal_aid, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, indian_state).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one civil framework through which any two Indian citizens can marry, register, divorce, and succeed regardless of religion, adjudicated by civil courts — solving the problem that no community law regime can marry an inter-religious couple without conversion, and that divorce and succession rights otherwise vary with the husband's community.
% TRANSFER_FUNCTION: Moves adjudication authority over marriage and divorce from community institutions to civil courts for the couples who opt in; moves notice, compliance, and community-exit costs onto those couples; moves registry integrity, constitutional legitimacy, and a uniform-civil-code template to the state.
% ABSENT_VOICES: Personal-law boards and community authorities would contest the civil track's legitimacy claims and resist its expansion, but hold no seat in its design — their objections surface only as political opposition outside the Act. The families and community elders of opting-in couples are likewise absent from the design conversation, yet the Act hands them a channel anyway: the public notice and objection window is precisely where their interference enters. The couples most endangered by that channel had no seat when the notice requirement was drafted and have none in its maintenance.
% DISAPPEARANCE_RATIONALE: Inter-religious couples would lose the only non-conversion path to a valid marriage and revert to conversion or non-recognition; women in several communities would lose the symmetric divorce and succession track; the state would lose its uniform registry and its working template for a uniform civil code; community regimes would absorb the displaced couples back into their own courts.
% FOUNDING_PROBLEM: At independence, India needed a marriage path that did not require religious conversion for inter-religious couples, and a civil framework answerable to constitutional individual rights rather than community authority; the Special Marriage Act 1954 (successor to the 1872 Act) was built to provide that path.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: Supreme Court and High Court case records (including the Safiya Sultana line, where couples sought the civil track precisely to avoid conversion and were obstructed by the notice regime), Law Commission consultation papers on marriage-law reform, and legal-aid documentation of interfaith couples — none of which is the state's own attestation.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46 is moderate: the arrangement delivers real coordination, but its own notice machinery imposes exposure costs concentrated on its most vulnerable users while the gains accrue to the state seat. Suppression 0.40 is a raw structural property (unscaled by power or scope in the engine's arithmetic): the arrangement suppresses no alternative regime — community law persists untouched as the default — but it operates coercive machinery inside the opt-in path (public notice, officer discretion). Theater 0.18 is low because the core functions (registration, adjudication, symmetric divorce) are real; what has grown performative is the objection window's fraud-prevention rationale — bigamy is prosecuted elsewhere, and documented objections overwhelmingly enable family interference rather than catch fraud. Accessibility_collapse 0.35 is low because the civil track collapses nothing: community law, religious ceremony, and conversion routes all remain fully available. Resistance 0.45 reflects political resistance from personal-law constituencies, community resistance directed at opting-in couples, and street-level administrative friction. Time points are years since the 1954 enactment (T=0 is 1954, T=72 is 2026); all three series share one grid. The suppression series is authored because enforcement capacity is the tracked dynamic: it traces a hump — statutory notice plus hardening street-level officer practice through the identity-politics decades (peak near T=36), then judicial relaxation after the constitutional privacy ruling and the Safiya Sultana line — the only metric that turns downward, and the only one the judiciary itself moved. The identity lock on community_exit_couples is relational and communal: the legal act of exiting is a signature, but self-concept, dispute-resolution, marriage-market standing, and ritual community remain inside the network; if that frame broke and exit carried no standing cost, their position would fall toward the beneficiary end and the arrangement's victim set would shrink to the notice-exposed subclass. Suppression here is overwhelmingly structural — the notice board, officer discretion, and community enforcement are external machinery; the internalized component (couples self-censoring in anticipation of the window) exists but is downstream of the structural mechanism, unlike interpersonal cases where internalization outlives the barrier. Note also the coalition structure: the Act's couple-unit design atomizes its users — each couple faces the notice window alone, and no standing class of civil-track couples exists with shared institutions; coalition power for the powerless seat is latent, not actual, and that atomization is itself part of why the exposure channel persists.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat the arrangement is a rights-fulfilling achievement it built and can expand at will; from the notice-exposed couple's seat the same notice board is the mechanism by which their marriage becomes a public target; from the judiciary's seat both are true at once, which is why the same institution administers the notice regime and has begun striking it down jurisdiction by jurisdiction. The payer seats and the agenda-setter seats should compute different types from the same structural data — that divergence is the measurement the corpus exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   indian_state appears in beneficiaries and holds the agenda_setter role with arbitrage-grade exit, so it derives near the beneficiary end: it collects legitimacy and registry gains while bearing only administration costs. interfaith_couples and notice_exposed_couples each carry BOTH declarations deliberately — they are net beneficiaries by revealed preference (the civil track is the only path they have), while the notice machinery extracts exposure from them specifically; their derived directionality should sit near symmetric, slightly target-side for the trapped subclass. community_exit_couples carry the victim declaration with identity_locked exit, placing them near the target end: the arrangement's benefit to them is front-loaded (a valid marriage) while its costs are standing (community standing, customary channels). women_seeking_gender_equal_divorce derive near the beneficiary end. personal_law_communities are authored as excluded rather than victims: the arrangement as it stands takes little from them — their objection is to its expansion, which is a contest about a different, larger arrangement. No directionality overrides are used; the dual declarations carry the mixed positions, and the derivation chain should resolve them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a non-conversion marriage path answerable to individual rights — is still live, corroborated by case records of couples who still need the track; status live crossed with verdict world_rearranges produces no mandatrophy mismatch, and no resolved-mandatrophy declaration is made. The tangled_rope claim is what prevents mislabeling in both directions: reading the arrangement as pure extraction would erase the interfaith couples and divorced women who owe their legal standing to it; reading it as pure coordination would erase the notice regime's documented exposure costs and the fact that the gains accrue to the state seat while the most vulnerable users pay. The cheap-fix observation sharpens the hybrid reading: the main extractive component (public notice) is legislatively trivial to repair and remains unrepaired, which suggests the exposure channel serves interests beyond fraud prevention — a coordination/extraction boundary held open by the notice_necessity omega rather than resolved by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the marriage_authority_kernel (the secular_civil_reading); what would the classification become if a sibling reading — e.g. muslim_shariat_reading or hindu_codified_reading — were instantiated instead, and is the family''s disagreement located in the locus of adjudication authority (civil court versus community institution) or in the rights-bearing unit (individual citizen versus community member)?',
    'The sibling files themselves: each reading is authored as a separate ε-invariant constraint; comparing victim sets, ε, and computed types across the family locates whether the structural delta between readings is adjudication locus, victim set, or both.',
    'If the delta is primarily victim-set (who bears unequal divorce and unilateral-exit costs), the family''s classifications should order by the gender equity of each regime; if primarily adjudication locus, the family should order by enforcement machinery — and different contamination networks follow from each resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this story is one reading of a contested kernel; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    notice_necessity,
    'Is the 30-day public notice requirement a necessary coordination cost (fraud, bigamy, and coercion screening), or an extractive exposure mechanism that confidential verification could replace?',
    'Natural experiment across High Courts that have held the notice dispensable after the constitutional privacy ruling: if fraud and bigamy rates do not rise where notice is waived while harassment incidents fall, the exposure buys no coordination.',
    'If dispensable, the arrangement''s dominant extraction channel closes and ε falls toward coordination-cost levels, moving the computed type toward rope; if necessary, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notice_necessity, empirical, 'Whether the notice regime is a genuine coordination cost or the arrangement''s main extraction channel.').

omega_variable(
    exit_cost_attribution,
    'The declared structural delta — social costs for exit from community law — are those costs imposed by this civil arrangement''s own design (public notice, publicity of civil marriage), by the community regimes the couple leaves, or jointly?',
    'Compare exit-cost trajectories for couples registering under confidentiality-protected procedures versus public-notice procedures within the same communities; and compare communities that socially sanction civil marriage with those that do not.',
    'If the costs are community-imposed, this reading''s ε should shed them (moving toward rope) and the sibling community readings should carry them; if the notice design amplifies them, the costs are internal to this arrangement and the tangled_rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_attribution, conceptual, 'Attribution of the community-exit cost structure between this arrangement and the sibling community regimes.').

omega_variable(
    ucc_scaffold_status,
    'Is the civil track a standing parallel arrangement whose opt-in design is the point, or a de facto transitional scaffold toward a uniform civil code whose justification is the transition?',
    'Legislative history and reform-commission trajectory: whether successive governments treat expansion of the civil track as the goal (scaffold-like) or maintain it as a permanent minority path (standing).',
    'A scaffold reading would subject the arrangement to sunset and transition analysis — its persistence without expansion would count as drift; the standing reading treats the parallel structure as stable by design and evaluates it as-is.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ucc_scaffold_status, conceptual, 'Whether the arrangement is transitional toward a uniform civil code or a standing parallel track.').

omega_variable(
    officer_discretion_share,
    'How much of the arrangement''s coercive force operates through street-level marriage-officer discretion (delay, summoning families, demands for proof) rather than through the statutory text itself?',
    'District-level audit of registration timelines and officer practices against the statutory requirements; litigation records where officer discretion was the operative barrier.',
    'If discretion-dominant, the suppression is administrative practice correctable by rule change without legislative amendment, and the suppression series'' recent judicial softening should continue; if statutory, only legislative change moves it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officer_discretion_share, empirical, 'Share of the arrangement''s coercive force carried by administrative discretion versus statutory design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secular_civil_reading_tr_t0, marriage_authority_kernel__secular_civil_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(secular_civil_reading_tr_t0, observed).
narrative_ontology:measurement(secular_civil_reading_tr_t12, marriage_authority_kernel__secular_civil_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement_basis(secular_civil_reading_tr_t12, observed).
narrative_ontology:measurement(secular_civil_reading_tr_t24, marriage_authority_kernel__secular_civil_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement_basis(secular_civil_reading_tr_t24, observed).
narrative_ontology:measurement(secular_civil_reading_tr_t36, marriage_authority_kernel__secular_civil_reading, theater_ratio, 36, 0.14).
narrative_ontology:measurement_basis(secular_civil_reading_tr_t36, observed).
narrative_ontology:measurement(secular_civil_reading_tr_t48, marriage_authority_kernel__secular_civil_reading, theater_ratio, 48, 0.15).
narrative_ontology:measurement_basis(secular_civil_reading_tr_t48, observed).
narrative_ontology:measurement(secular_civil_reading_tr_t60, marriage_authority_kernel__secular_civil_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(secular_civil_reading_tr_t60, observed).
narrative_ontology:measurement(secular_civil_reading_tr_t72, marriage_authority_kernel__secular_civil_reading, theater_ratio, 72, 0.18).
narrative_ontology:measurement_basis(secular_civil_reading_tr_t72, observed).

% Extraction over time
narrative_ontology:measurement(secular_civil_reading_be_t0, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(secular_civil_reading_be_t0, observed).
narrative_ontology:measurement(secular_civil_reading_be_t12, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement_basis(secular_civil_reading_be_t12, observed).
narrative_ontology:measurement(secular_civil_reading_be_t24, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement_basis(secular_civil_reading_be_t24, observed).
narrative_ontology:measurement(secular_civil_reading_be_t36, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 36, 0.44).
narrative_ontology:measurement_basis(secular_civil_reading_be_t36, observed).
narrative_ontology:measurement(secular_civil_reading_be_t48, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 48, 0.45).
narrative_ontology:measurement_basis(secular_civil_reading_be_t48, observed).
narrative_ontology:measurement(secular_civil_reading_be_t60, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement_basis(secular_civil_reading_be_t60, observed).
narrative_ontology:measurement(secular_civil_reading_be_t72, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 72, 0.46).
narrative_ontology:measurement_basis(secular_civil_reading_be_t72, observed).

% Suppression requirement over time
narrative_ontology:measurement(secular_civil_reading_su_t0, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(secular_civil_reading_su_t0, observed).
narrative_ontology:measurement(secular_civil_reading_su_t12, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement_basis(secular_civil_reading_su_t12, observed).
narrative_ontology:measurement(secular_civil_reading_su_t24, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(secular_civil_reading_su_t24, observed).
narrative_ontology:measurement(secular_civil_reading_su_t36, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 36, 0.48).
narrative_ontology:measurement_basis(secular_civil_reading_su_t36, observed).
narrative_ontology:measurement(secular_civil_reading_su_t48, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 48, 0.47).
narrative_ontology:measurement_basis(secular_civil_reading_su_t48, observed).
narrative_ontology:measurement(secular_civil_reading_su_t60, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement_basis(secular_civil_reading_su_t60, observed).
narrative_ontology:measurement(secular_civil_reading_su_t72, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 72, 0.4).
narrative_ontology:measurement_basis(secular_civil_reading_su_t72, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, parsi_communal_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who governs marriage in India' decomposes, per the ε-invariance principle, into five structurally distinct authority arrangements — one per reading of the marriage_authority_kernel — each with its own ε, victim set, and adjudicating institution. This file is the secular civil reading. It sits upstream of the hindu and muslim codified readings in one specific sense: couples exit those regimes into this one (succession displacement under the civil Act's succession rule; constitutional review pressure on community practices), while the community readings remain the default for most marriages. The family's ε ordering should track each regime's gender-equity and exit-cost structure; this reading carries the highest equity and the least internal coercion of the five, but is the only one whose machinery publishes its users' identities in advance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
