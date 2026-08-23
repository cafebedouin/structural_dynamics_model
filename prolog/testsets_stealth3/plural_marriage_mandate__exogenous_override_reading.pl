% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: Federal Anti-Polygamy Enforcement and the 1890 Manifesto (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto suspended new plural marriages in the LDS Church after
 *   twenty-eight years of escalating federal enforcement — the Morrill Act
 *   (1862), the Poland Act (1874), the Edmunds Act (1882), and the
 *   Edmunds-Tucker Act (1887), which dissolved the church's charter and
 *   escheated its property. This story instantiates the exogenous_override
 *   reading of the plural_marriage_mandate kernel: on this reading the
 *   Manifesto was the completion of a coercive federal campaign that forced
 *   abandonment of a requirement the church held divine, and the 'voluntary
 *   revelation' framing is the arrangement's cover story, not its substance.
 *   The standing arrangement under contest — the referent of every metric
 *   below — is that enforcement regime together with the abandonment it
 *   produced, assessed by this reading's own lights. Per the claim/metric
 *   independence rule, claimed_type is authored from this reading's
 *   structural claim (coercive extraction masked as voluntary compliance) and
 *   the metrics are authored as descriptively true of the arrangement's
 *   operation; the engine computes per-seat classifications from the
 *   structural data, and any divergence between claim and computed type is
 *   the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - us_federal_government: agenda-setter and primary beneficiary (institutional/arbitrage) — built the enforcement machinery across four statutes, collects conformity, escheated property, and precedent, and can dissolve the arrangement at will
 *   - practicing_polygamists: primary target (powerless/constrained) — bore imprisonment, disenfranchisement, and family rupture; no seat in the 1890 decision
 *   - lds_church_institution: dual-positioned payer-beneficiary (institutional/identity_locked) — lost charter, temples, and doctrine; gained survival, statehood, and legitimation
 *   - lds_rank_and_file_believers: target (powerless/identity_locked) — lost franchise and tithing to the fight, inherited the doctrinal rupture
 *   - utah_federal_enforcement_apparatus: secondary beneficiary (institutional/mobile) — salaries, fees, and careers flowed through the prosecutions
 *   - anti_polygamy_reform_coalition: secondary beneficiary (organized/mobile) — collected vindication of its civilizational program at zero enforcement cost
 *   - mexican_canadian_colony_settlers: excluded voice (moderate/mobile) — purchased exit through emigration and were stranded by a decision they were never consulted on
 *   - religious_history_analysts: analytical observer (analytical/analytical) — the standing outside check on both official accounts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.78).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.72).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "Federal Anti-Polygamy Enforcement and the 1890 Manifesto (Exogenous Override Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '1acd7a93-9e98-4aff-85d6-09e8f0890ad9').
narrative_ontology:cs_kernel_codification('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', fixed_text).
narrative_ontology:cs_authority_grounding('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', extraction).
narrative_ontology:cs_interpretation_layer_present('1acd7a93-9e98-4aff-85d6-09e8f0890ad9').
narrative_ontology:cs_reading_relation('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', foundational, divine_command_irrepealable_by_secular_power).
narrative_ontology:cs_axiom_status(divine_command_irrepealable_by_secular_power, holdable).
narrative_ontology:cs_axiom_grounding('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', divine_command_irrepealable_by_secular_power, theological).
narrative_ontology:cs_axiom('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', foundational, duress_extracted_abandonment_carries_no_doctrinal_authority).
narrative_ontology:cs_axiom_status(duress_extracted_abandonment_carries_no_doctrinal_authority, holdable).
narrative_ontology:cs_axiom_grounding('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', duress_extracted_abandonment_carries_no_doctrinal_authority, deontological).
narrative_ontology:cs_reference_frame('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', immutable_divine_command_frame).
narrative_ontology:cs_drift_state('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', post_smoot_settlement, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('1acd7a93-9e98-4aff-85d6-09e8f0890ad9', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, us_federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, utah_federal_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_reform_coalition).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, lds_church_institution).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, lds_rank_and_file_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, lds_church_institution).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, monogamy_as_civilizational_standard).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, federal_supremacy_over_territorial_religion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress and the federal courts built the anti-polygamy machinery across four decades: the Morrill Act, the Poland Act, the Edmunds Act with its test oath and marshal surge, and the Edmunds-Tucker Act dissolving the church's charter and escheating its property. It set the terms, prosecuted under them, and conditioned Utah statehood on conformity. It collects the escheated assets, the territorial conformity, and the precedent that federal law overrides territorial religious practice, and it can dissolve the whole arrangement by standing down — it faces no exit problem of its own.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, us_federal_government, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, us_federal_government, beneficiary).

% Territorial governors, federal judges, U.S. marshals, and the Utah Commission staffed the prosecutions, the disfranchisement boards, and the property proceedings. Salaries, fees, forfeitures, and careers flowed through the enforcement campaign; when the campaign wound down after statehood, appointees rotated to other posts. They ran the machinery; they did not set its terms.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, utah_federal_enforcement_apparatus, beneficiary,
    institutional, biographical, mobile, regional).

% Protestant denominations, women's reform organizations, and the Republican Party supplied the campaign's moral energy and electoral muscle. Victory vindicated their civilizational program — monogamy as the mark of republican society — and became a party credential. They bore none of the enforcement's costs and redirected their attention to other causes once Utah conformed.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_reform_coalition, beneficiary,
    organized, generational, mobile, national).

% Men with plural families faced indictment, prison terms of months to years, and loss of the vote. Some served the sentences rather than abandon wives and children; others uprooted entire households to colonies in Chihuahua or Alberta to live under foreign law. Their families bore the separations and the poverty either way. No seat existed for them in the 1890 decision that ended the practice they had sacrificed to build.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, constrained, continental).

% The church lost its charter, its temples and real property to escheatment, and its members' franchise; by capitulating it gained survival, statehood, and national legitimation. Its self-concept was fused with the covenant practice and the persecuted-remnant story, so surrendering the practice under federal pressure broke against its own account of what it was. It could not keep the practice without dissolution and could not abandon it without repudiating its own past claims — it had no exit that left it intact, so it absorbed the rupture and reframed the surrender as divine initiative.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, lds_church_institution, payer,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, lds_church_institution, beneficiary).

% Ordinary members lost voting rights under the test oaths, funded the legal defense through tithing, and inherited the doctrinal rupture: a commandment they had been taught was required for exaltation was withdrawn by the same authority that had pronounced it eternal. Leaving the faith meant losing their entire social and salvific world, so they stayed, absorbed the reversal, and passed down the official account of it.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, lds_rank_and_file_believers, payer,
    powerless, generational, identity_locked, continental).

% Families who had already emigrated to Chihuahua and Alberta to keep the practice alive under foreign jurisdiction. The Manifesto was issued without their consent and severed the arrangement they had uprooted themselves to preserve; their sacrifice purchased no exemption and their voice was never sought. Some returned; most stayed and built towns that outlasted the practice that founded them.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, mexican_canadian_colony_settlers, excluded,
    moderate, generational, mobile, continental).

% Historians of American religious history and law reconstruct the causal chain from court records, diaries, and hearing testimony. They hold no stake in the covenant and adjudicate nothing; their accounts are the standing outside check on both the church's official narrative and the government's self-description.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, religious_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, us_federal_government).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Imposed a single, federally enforceable marriage standard across the territories and resolved the sovereignty conflict between a territorial religious polity and the federal republic — aligning Utah's legal order with the national monogamous norm as the price of statehood.
% TRANSFER_FUNCTION: Moved liberty (prison terms for hundreds of practitioners), property (escheated church charter, temples, and real estate), citizenship rights (disenfranchisement under test oaths), and finally the covenant practice itself, from church members and the church institution to the federal government and the national monogamous order.
% ABSENT_VOICES: The practitioners who served prison terms, the families already exiled to the Mexican and Canadian colonies, and rank-and-file believers who held the practice a binding commandment had no seat in the 1890 decision; nor did the women in plural marriages whose status the reformers claimed to defend but who were not consulted. Their objection — that a commandment held eternal cannot be rescinded by statute, or by an authority acting under statutory duress — was never adjudicated within the arrangement.
% DISAPPEARANCE_RATIONALE: Without the enforcement regime, plural marriage practice would have continued openly, Utah statehood would have arrived later or on different terms, the Mexican and Canadian colonies would not exist in their formed shape, the church's post-1890 institutional trajectory (the Smoot era and national integration) would not have run as it did, and the national marriage-law standard would have developed with a live territorial exception.
% FOUNDING_PROBLEM: The conflict between a territorial religious community practicing what it held to be divinely mandated plural marriage and the federal republic's monogamous legal order — federal sovereignty against territorial theocratic self-governance.
% FOUNDING_PROBLEM_CORROBORATION: Federal court records and the Supreme Court's own opinions (Late Corporation of the Church of Jesus Christ of Latter-day Saints v. United States, 1890) attest the enforcement conflict and its terms; contemporaneous congressional debate and the 1904-1907 Smoot hearing record attest that the conflict was resolved by capitulation rather than doctrinal persuasion; non-LDS historians of the period treat the Manifesto as a response to imminent federal destruction. The church itself attests a different founding story (revelation, not coercion) — that dispute is the kernel contest, routed to the omegas — and no source outside the beneficiary set attests that the abandonment was voluntary.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.78 (end-state): over the interval the arrangement took liberty (several hundred practitioners imprisoned), property (charter, temples, and real estate escheated under Edmunds-Tucker), citizenship rights (disenfranchisement under test oaths), and finally the covenant practice itself. Suppression is authored at 0.72 as a raw structural property — unscaled by power or scope, per the framework's rule; the standing arrangement is held by federal criminal law plus the church's own post-settlement disciplinary machinery, which replaced the marshals after 1907. Theater ratio 0.55: after the Second Manifesto closed covert practice, the arrangement's ongoing maintenance is substantially narrative — the official account of voluntary revelation — over function, which is now internal enforcement of the abandonment; the 1890-1904 peak reflects the widest gap between public conformity performance and covert practice. Accessibility collapse 0.60: exit was real and used (the Mexican and Canadian colonies), and the prison-martyrdom path persisted; alternatives partly survived, which is why the regime needed twenty-five years of escalating force rather than one statute. Resistance 0.70: hundreds chose prison, Reynolds v. United States (1879) tested the regime at the Supreme Court, and the church litigated and delayed for a generation. The measurement series run on one shared grid (1862, 1874, 1882, 1887, 1890, 1896, 1904, 1907) with every tracked metric authored at every point; the suppression_requirement series is authored because this story specifically traces enforcement-capacity change — build-up to the 1890 peak, partial stand-down at statehood, renewal under the Smoot hearings, settlement. Coalition note: the payer seats' coalition vehicle was the church itself; its capitulation dissolved the coalition, and the colonists' emigration was the other collective move — both were absorbed or bypassed, which is part of why the arrangement held.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the federal government's position the arrangement is legitimate law enforcement achieving constitutional conformity — the coordination story is real from where it sits. From the practitioner and believer seats the same structure is the taking of a covenant under threat of prison and confiscation. The church's seat is genuinely dual: it paid charter, property, and doctrine, and collected survival, statehood, and legitimation — which is why its public account (revelation) serves both its loss and its gain. The engine computes per-seat classifications from power, exit, and declared roles; the authored snare claim does not adjudicate the divergence — it is this reading's structural claim, offered independently of the metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (us_federal_government, utah_federal_enforcement_apparatus, anti_polygamy_reform_coalition) place those seats near the beneficiary end: the federal seat collects conformity and escheated assets with arbitrage-grade control of the arrangement itself; the enforcement apparatus collected salaries and careers and rotated out; the reform coalition collected vindication at zero cost. Victim declarations (practicing_polygamists, lds_rank_and_file_believers, lds_church_institution) place those seats near the target end; identity_locked exit on the two believer seats pushes them toward the full-target end — they could not leave without ceasing to be what they were. The church's dual position (payer with a beneficiary secondary role) should derive mid-to-high directionality; its identity lock keeps it off the beneficiary end despite the statehood gain. Continental scope amplifies effective extraction modestly through verification difficulty; suppression is not scaled by anything — it is authored as the raw structural fact of test oaths, marshals, and escheatment. No directionality overrides are used: the role and exit declarations carry the asymmetry, and the schema's override is power-atom-grained, which would conflate the two institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification keeps the coercion visible against the arrangement's own cover story: reading the post-1890 quiet as consent would erase the victim set, and reading the Manifesto as legitimate reinterpretation (the endogenous sibling's move) would dissolve the victims into beneficiaries of a blessing. The R5 interview carries the zombie signal: the founding problem — federal sovereignty versus territorial theocratic marriage practice — was resolved by 1896-1907, the enforcement machinery largely stood down, and yet the arrangement persists, now maintained by narrative and internal discipline rather than by the coercion that built it. founding_problem_status dead plus disappearance_verdict world_rearranges is the mismatch profile: the world did rearrange around the taking, and the taking stays taken after the taker's active interest lapsed. The classification prevents mislabeling that residue as either pure coordination (rope) or pure performance (piton): the taking was real, completed, and is now held in place mostly by the story told about it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading of the plural_marriage_mandate kernel. Do the sibling readings, computed over the same standing arrangement, classify it differently — and is the snare classification stable across the kernel or specific to this reading?',
    'Compile the sibling files (plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading) and compare story-level and per-seat classifications; the endogenous file should show a dissolved victim set and extraction near coordination cost; the pragmatism file should show the church leadership seat as capturer of the arrangement''s gains.',
    'If the snare type appears only under this reading, the classification is reading-indexed (as OQ-26 predicts) and the kernel''s causal contest is the real object of study; if all three compute snare-like structure, the causal dispute is secondary to the coercive structure itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of the classification across the kernel''s sibling readings.').

omega_variable(
    revelation_coercion_underdetermination,
    'Was the Manifesto''s proximate cause a divine revelation to Wilford Woodruff, or the imminent coercive destruction of the church — pending escheatment enforcement, prospective disenfranchisement of the whole membership, and failure of statehood?',
    'Woodruff''s contemporaneous diaries set against his later public accounts, the two 1890 speeches record, testimony before the 1904-1907 Smoot hearings, and the timeline of court decisions and pending legislation.',
    'If the record shows coercion-dominance, this reading''s snare claim holds its causal anchor; if revelation-dominance were established, the endogenous sibling''s file would govern and this story''s victim set would dissolve into a legitimately suspended practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_coercion_underdetermination, empirical, 'Causal attribution of the Manifesto: revelation versus coercion.').

omega_variable(
    post_manifesto_practice_extent,
    'How extensive were the covert post-Manifesto plural marriages (1890-1904), some authorized by senior church leadership, that continued while the church publicly conformed?',
    'Archival sealing and temple records, genealogical reconstruction of post-1890 plural families, and the contemporaneous investigations that produced the Second Manifesto.',
    'Greater extent raises the theater ratio (public compliance diverged further from practice) and weakens the voluntariness performance; it also shows the abandonment was less complete in 1890 than the official account claims, tightening the coercion timeline to 1904.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_manifesto_practice_extent, empirical, 'Extent of covert post-Manifesto practice versus public conformity.').

omega_variable(
    victim_set_scope,
    'Does the victim set include the abandoned covenant itself — the practice as a collective religious good held by all believing members — or only the directly prosecuted individuals and the seized corporate property?',
    'Depends on whether divinely commanded practices are modeled as held goods that can be taken from a community, or only as individual liberties and property rights; this reading models them as held goods, the pragmatism sibling does not.',
    'The broader victim set sustains high extraction across the whole believer population; the narrower set concentrates the taking on the prosecuted and dispossessed and leaves the rank-and-file seat near-symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_scope, conceptual, 'Scope of the victim set: covenant-as-held-good versus individual liberties and property.').

omega_variable(
    suppression_locus_after_settlement,
    'After 1907, is the standing arrangement''s suppressive force maintained by federal criminal law and the memory of coercion (structural), or by the church''s own disciplinary machinery and the believers'' internalized commitment (internalized)?',
    'Observe enforcement of the abandonment after federal attention receded: post-settlement disciplinary councils, temple-interview screening, and the church''s own prosecution of new plural unions.',
    'If the locus is internalized, the arrangement persists even under full federal withdrawal — the zombie structure the R5 mismatch flags — and the suppression metric understates the standing arrangement''s hold; if structural, renewed federal disengagement would eventually reopen the practice question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_locus_after_settlement, empirical, 'Locus of post-settlement suppression: structural federal force versus internalized institutional discipline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1862, 1907).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmm_exog_override_tr_t1862, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1862, 0.08).
narrative_ontology:measurement(pmm_exog_override_tr_t1874, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1874, 0.1).
narrative_ontology:measurement(pmm_exog_override_tr_t1882, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1882, 0.14).
narrative_ontology:measurement(pmm_exog_override_tr_t1887, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1887, 0.2).
narrative_ontology:measurement(pmm_exog_override_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.48).
narrative_ontology:measurement(pmm_exog_override_tr_t1896, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1896, 0.56).
narrative_ontology:measurement(pmm_exog_override_tr_t1904, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1904, 0.62).
narrative_ontology:measurement(pmm_exog_override_tr_t1907, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1907, 0.55).

% Extraction over time
narrative_ontology:measurement(pmm_exog_override_be_t1862, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1862, 0.22).
narrative_ontology:measurement(pmm_exog_override_be_t1874, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1874, 0.3).
narrative_ontology:measurement(pmm_exog_override_be_t1882, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1882, 0.55).
narrative_ontology:measurement(pmm_exog_override_be_t1887, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1887, 0.78).
narrative_ontology:measurement(pmm_exog_override_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.85).
narrative_ontology:measurement(pmm_exog_override_be_t1896, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1896, 0.81).
narrative_ontology:measurement(pmm_exog_override_be_t1904, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1904, 0.79).
narrative_ontology:measurement(pmm_exog_override_be_t1907, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1907, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pmm_exog_override_su_t1862, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1862, 0.12).
narrative_ontology:measurement(pmm_exog_override_su_t1874, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1874, 0.25).
narrative_ontology:measurement(pmm_exog_override_su_t1882, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1882, 0.55).
narrative_ontology:measurement(pmm_exog_override_su_t1887, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1887, 0.82).
narrative_ontology:measurement(pmm_exog_override_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(pmm_exog_override_su_t1896, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1896, 0.7).
narrative_ontology:measurement(pmm_exog_override_su_t1904, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1904, 0.76).
narrative_ontology:measurement(pmm_exog_override_su_t1907, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1907, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The kernel plural_marriage_mandate decomposes into three reading-stories over one standing arrangement (the enforced abandonment). The readings differ on the causal and normative status of the 1890 event, which changes the victim and beneficiary sets: endogenous (no victims, suspension as gift), exogenous_override (this file: practitioners and believers as victims, federal state as beneficiary), pragmatism (church leadership as capturer of its own cover story's proceeds). The files are linked, not merged; epsilon is reading-indexed over the shared referent per OQ-26.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
