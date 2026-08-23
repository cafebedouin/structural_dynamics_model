% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country, Two Systems Framework - Autonomy-Primacy Reading
 *   domain: constitutional/political/legal
 *
 * SUMMARY:
 *   The 1984 Sino-British Joint Declaration and the 1990 Basic Law codified a
 *   promise: after the 1997 handover, Hong Kong keeps its common-law courts,
 *   open economy, and civil liberties for fifty years, with the ultimate aim
 *   of universal suffrage written into the Basic Law's own articles. This
 *   story instantiates the autonomy-primacy reading of that commitment - the
 *   framework as a treaty-backed guarantee whose breach by the centre is a
 *   measurable violation, not a prerogative. On the shared referent of the
 *   standing arrangement, the three readings of the
 *   one_country_two_systems_framework kernel author different constraints
 *   with different epsilon: this reading locates the burden in the surrender
 *   of autonomy and civic rights by Hong Kong residents and institutions to
 *   central control (concentrated on political actors, low for most
 *   residents' daily life, per the expected structural delta); the
 *   sovereignty-primacy reading locates essentially none at the centre and
 *   treats intervention as lawful administration of delegated authority; the
 *   balanced-coexistence reading treats the burden as whatever the current
 *   negotiation balance happens to yield. The claim/metrics gap is deliberate
 *   and load-bearing: the constraint is CLAIMED as scaffold (a time-bounded
 *   transitional guarantee carrying a declared reform destination and a
 *   terminal date), while the authored metrics describe an operation that has
 *   hardened into enforced, asymmetric extraction since 2020 - the divergence
 *   between claim and operation is the datum this story contributes.
 *
 * KEY AGENTS:
 *   - prc_central_authorities: agenda-setting sovereign (institutional/arbitrage) - drafts, interprets, and enforces the framework's boundaries; collects stability and gateway rents; faces no external enforcement forum
 *   - hong_kong_general_public: intended beneficiary turned dual-positioned (organized/constrained) - receives the rule-of-law envelope, pays in narrowed expression and, for a subset, prosecution
 *   - pro_democracy_activists: primary target (organized/trapped) - coalition builders now facing pending subversion prosecutions and travel restrictions
 *   - opposition_legislators: target (moderate/trapped) - disqualified en masse, chamber reduced to vetted token opposition
 *   - independent_press_organisations: target (moderate/trapped) - flagship newsroom closed by asset freeze and arrest; survivors under legal exposure
 *   - hong_kong_emigres: exited targets (moderate/arbitrage) - absorbed the cost, then left; carry sunk cost and residual self-censorship
 *   - hong_kong_judiciary: administering check under squeeze (institutional/identity_locked) - review still functions for ordinary matters; security matters ring-fenced; institution cannot exit without dissolving itself
 *   - hong_kong_financial_establishment: concentrated beneficiary (powerful/arbitrage) - prospered throughout; exposure reversible via foreign assets
 *   - international_financial_institutions: beneficiary (institutional/arbitrage) - rely on distinct-system guarantees; maintain parallel hubs elsewhere
 *   - united_kingdom_cosignatory: monitoring observer (institutional/analytical) - co-signatory with reporting duty and no enforcement lever
 *   - us_allied_sanctions_coalition: external observer (institutional/analytical) - retaliates against individuals after the fact; does not restore guarantees
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.66).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.74).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, scaffold).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country, Two Systems Framework - Autonomy-Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional/political/legal").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:has_sunset_clause(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, 'd0fee873-4276-4988-936f-0d66c501cddf').
narrative_ontology:cs_kernel_codification('d0fee873-4276-4988-936f-0d66c501cddf', formalized).
narrative_ontology:cs_authority_grounding('d0fee873-4276-4988-936f-0d66c501cddf', lineage).
narrative_ontology:cs_interpretation_layer_present('d0fee873-4276-4988-936f-0d66c501cddf').
narrative_ontology:cs_reading_relation('d0fee873-4276-4988-936f-0d66c501cddf', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d0fee873-4276-4988-936f-0d66c501cddf', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('d0fee873-4276-4988-936f-0d66c501cddf', foundational, central_intervention_constitutes_treaty_breach).
narrative_ontology:cs_axiom_status(central_intervention_constitutes_treaty_breach, holdable).
narrative_ontology:cs_axiom_grounding('d0fee873-4276-4988-936f-0d66c501cddf', central_intervention_constitutes_treaty_breach, conventional).
narrative_ontology:cs_axiom('d0fee873-4276-4988-936f-0d66c501cddf', foundational, universal_suffrage_is_entitled_destination).
narrative_ontology:cs_axiom_status(universal_suffrage_is_entitled_destination, holdable).
narrative_ontology:cs_axiom_grounding('d0fee873-4276-4988-936f-0d66c501cddf', universal_suffrage_is_entitled_destination, deontological).
narrative_ontology:cs_reference_frame('d0fee873-4276-4988-936f-0d66c501cddf', treaty_guaranteed_substantive_autonomy).
narrative_ontology:cs_drift_state('d0fee873-4276-4988-936f-0d66c501cddf', post_national_security_law_settlement, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('d0fee873-4276-4988-936f-0d66c501cddf', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authorities).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_general_public).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_financial_establishment).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_financial_institutions).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, opposition_legislators).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, independent_press_organisations).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_emigres).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_general_public).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, joint_declaration_registered_treaty_obligation).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, basic_law_mini_constitution_supremacy_within_hk).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, judicial_review_executive_check_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, iccpr_continuation_via_bill_of_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sovereign government of the People's Republic of China. Drafted and promulgated the Basic Law, retains sole interpretive authority through the NPC Standing Committee, imposed the 2020 National Security Law directly into Hong Kong law, and restructured Hong Kong's electoral system in 2021 around a candidacy-review committee. Collects stability and prosperity gains from Hong Kong's functioning as a financial gateway while treating national security and sovereignty as a reserved sphere outside local autonomy. No external forum can compel answers regarding its treaty conduct; its working escapes from the framework's limits are interpretations and legislative impositions of its own making.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authorities, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authorities, beneficiary).

% Seven-plus million residents. Receive common-law courts, open markets, travel freedoms, and a distinct civic life underwritten by the framework. Since 2020 a subset - protesters, candidates, union leaders - absorbs arrests, disqualifications, and prosecution, while the wider public absorbs a narrowing of what can safely be said, taught, or published. Emigration is possible and hundreds of thousands have taken it, but leaving means abandoning property, family networks, and careers, so exit is costly rather than closed.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_general_public, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_general_public, payer).

% Organizers, district councillors, and movement participants from the 2014 and 2019 mobilizations. Built the coalition infrastructure - primaries, unions, rallies - that won supermajority vote shares before 2021. Under the National Security Law dozens face subversion and collusion charges with trials pending for years, bail conditions restricting movement, and surrendered travel documents; departure is legally unavailable to many of them for the duration.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_activists, payer,
    organized, biographical, trapped, regional).

% Pan-democratic lawmakers. Held roughly half the elected chamber through 2020, resigned en masse in November 2020 after Beijing-authorized disqualification rulings, and saw the 2021 electoral overhaul cut opposition presence to a token few seats behind candidacy vetting. Several former members are detained or awaiting trial; returning to elected office requires passing the same vetting that removed them.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, opposition_legislators, payer,
    moderate, biographical, trapped, regional).

% Editors, publishers, and newsrooms, exemplified by Apple Daily. Published accountability journalism under the framework's press-freedom guarantees until the National Security Law enabled asset freezes, executive and reporter arrests, and the paper's forced closure in 2021. Surviving outlets operate under standing legal exposure that makes investigative coverage of security-related topics effectively unpublishable.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, independent_press_organisations, payer,
    moderate, biographical, trapped, regional).

% The post-2020 emigration wave - hundreds of thousands who left under British National (Overseas) and comparable visa routes. Sold or abandoned homes, rebuilt careers abroad, and separated families in the process. Having completed their exit they are spared further measures, but they carry the sunk cost and, according to emigrant surveys, much of the lingering self-censorship that traveled with them.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_emigres, payer,
    moderate, biographical, arbitrage, global).

% Hong Kong's courts and magistracy. Administer the framework daily by adjudicating Basic Law disputes and reviewing government action under the Bill of Rights; since 2020 they operate alongside National Security Law designation rules for sensitive cases, and senior overseas judges have resigned citing the changed environment. The bench as an institution cannot relocate or dissolve - exiting the arrangement would mean ending the very legal continuity it exists to preserve - so individuals leave while the institution stays and absorbs the pressure.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, payer).

% Property conglomerates, banks, and listed-company controlling shareholders headquartered in Hong Kong. Prospered continuously under the framework's stable, low-tax, common-law environment; continued operating through the post-2020 tightening and complied with oath and vetting requirements where requested. Hold diversified foreign assets and residences that keep their exposure to any single jurisdiction reversible.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_financial_establishment, beneficiary,
    powerful, generational, arbitrage, global).

% Global exchanges, asset managers, and professional-services firms using Hong Kong as a listing venue, offshore renminbi hub, and dispute-resolution seat. Their business case rests on the distinct-system guarantees; they maintain parallel hubs in Singapore, New York, and London, so relocating functions is expensive but executable if the guarantees degrade past a threshold.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Co-signatory of the 1984 Joint Declaration. Monitors compliance through six-monthly Foreign Office reports, widened BN(O) visa access in response to the 2020 security law, and publicly maintains that the declaration remains a binding registered treaty. It commands no enforcement mechanism; its censure operates as diplomatic record rather than legal sanction.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, united_kingdom_cosignatory, observer,
    institutional, biographical, analytical, continental).

% The United States and allied governments applying targeted measures - the Hong Kong Autonomy Act, Magnitsky-style designations - against officials they assess as responsible for dismantling the autonomy guarantees. Their instruments penalize named individuals after the fact and publish findings that the framework's terms have been materially altered; they neither compel restoration nor participate in the framework's own decision processes.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, us_allied_sanctions_coalition, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authorities).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a sovereignty transfer that would otherwise destroy value on both sides: it keeps one city running two systems - common-law courts, separate currency and customs territory, open capital account - inside a unitary socialist state, and gives investors a written, internationally registrable commitment device spanning the transition. The Basic Law's reform articles additionally coordinate expectations about where the arrangement is headed.
% TRANSFER_FUNCTION: Formally moved sovereignty from the United Kingdom to the People's Republic of China in 1997 while flowing autonomy protections downward to Hong Kong residents. In current operation the political dimension of the flow has reversed: candidacy control, security jurisdiction, and interpretive authority move upward from Hong Kong institutions and voters to central bodies, while economic privilege continues flowing to the established business elite and international finance.
% ABSENT_VOICES: Hong Kong voters are the missing seat: the universal-suffrage promise of Basic Law Articles 45 and 68 has no carrier in the post-2021 vetted chamber, the pan-democratic bloc that consistently won the majority of votes cast holds zero elected seats, and prosecuted or exiled activists participate only from detention dockets and diaspora broadcasts. They are absent because vetting, disqualification, and prosecution removed them - not because they stopped objecting. Taiwan watches the framework as the implicit test case for its own future.
% DISAPPEARANCE_RATIONALE: If the framework and its guarantees vanished overnight, Hong Kong's legal system, currency and customs arrangements, listing-venue status, and treaty-registered commitments would all lose their foundation simultaneously: courts would lose the constitutional text they adjudicate under, markets would immediately reprice the jurisdiction, and the residency and citizenship decisions of hundreds of thousands of people made in reliance on the framework would be stranded. Every seat named in this story holds arrangements that depend on it.
% FOUNDING_PROBLEM: How to return a treaty-acquired capitalist enclave to Chinese sovereignty without collapsing its economic function or triggering mass flight - answered in 1984 by codifying fifty years of preserved systems and a written autonomy guarantee registered as an international instrument.
% FOUNDING_PROBLEM_CORROBORATION: The transfer half is corroborated by everyone: the handover occurred and the systems persisted. The guarantee half is disputed - and attested from outside the beneficiary set by United Kingdom Foreign Office six-monthly reports asserting continuing treaty obligation, United Nations Human Rights Committee concluding observations on Hong Kong under the ICCPR, and public statements of the Hong Kong Bar Association; none of these parties collects from the arrangement's current operation. No source outside the beneficiary set attests that the guarantee remains fully operative.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.66 at interval end) is concentrated rather than diffuse: most residents' daily civil liberties remain comparatively low-cost while political participation, press freedom, and institutional independence absorb the burden - matching this reading's structural delta. Suppression (0.74) is the largest metric because the post-2020 settlement persists through continuous legal coercion - prosecutions, candidacy vetting, media closure - rather than voluntary assent; suppression is authored raw and unscaled per the framework's rule that only extractiveness is context-scaled. Theater (0.56) rises because the 'meaningful checks' increasingly perform: vetting keeps elections formally competitive while removing opposition viability, and consultative exercises continue without observable effect. Accessibility collapse (0.58) is moderate: recognizing that the guarantee lacks an enforcement forum closes off belief in legal remedies, though emigration remains a partially open alternative. Resistance (0.70) reflects the largest sustained mobilizations in the framework's history (2003, 2014, 2019) and the deliberate targeting of coalition infrastructure - the first major national-security prosecutions began with the organizers of an opposition primary, which is the coalition-power consideration for otherwise outmatched actors. All three tracked series run on one shared nine-point grid (1984-2026) with every metric authored at every point; the trajectories are monotonic ratchets, not cycles - each crisis (Article 23 2003, the 831 decision 2014, the extradition bill 2019) ended with the settlement permanently harder - so no cyclical-measurement apparatus applies.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently, and the engine computes that divergence from structural data. From the centre's seat the arrangement reads as orderly delegation being refined toward sovereignty - that is its own sibling reading - so the identical acts counted here as violations count there as administration. From the judiciary's seat the structure is a squeeze: judicial review still constrains executive action on ordinary matters while security matters are ring-fenced away from it. From the payer seats - defendants awaiting trial, disqualified lawmakers, a closed newsroom - the structure is the broken promise itself. The authored scaffold claim adjudicates none of this; it records what this reading believes the arrangement structurally is.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the centre collects stability and financial-gateway rents; residents receive the rule-of-law envelope; the financial establishment and international finance collect the business environment. Victim declarations push d toward the target end for prosecuted activists, removed legislators, shuttered press, and the emigrants who absorbed the cost before exiting. The centre is the ambiguous seat: listed among beneficiaries because it demonstrably collects, but its revealed conduct - five interpretations and counting, the 2021 white papers, direct imposition of security law - signals it experiences the binding guarantee as a net cost, pulling its effective d up from the derived beneficiary-low value. No directionality override is authored: overrides key on the power atom, and the institutional atom is shared by radically different seats (centre, courts, treaty guarantors, global finance), so a single correction would smear across positions it does not fit. The ambiguity is routed to omega prc_net_directionality_ambiguity instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim disciplines both mislabelings. Reading the arrangement as pure coordination erases the asymmetric burden the current operation places on civic actors; reading it as pure predation erases the genuine transitional achievement - the framework really did carry a sovereignty handover that preserved a functioning distinct system, and its articles really do name a reform destination and a terminal date. What the scaffold claim predicts is decay behavior: if the suffrage pathway dies (omega suffrage_pathway_liveness) while the 2047 horizon extends without renegotiation (omega terminal_date_meaning), the mandate outlives its function and the structure slides toward theatrical maintenance of a dead promise - the rising theater series is the leading indicator of exactly that slide. The R5 interview records the founding problem as solved-but-disputed and corroborates from outside the beneficiary set, which is what keeps this a contested mandate rather than a settled zombie; the mismatch consumer should watch status=contested against verdict=world_rearranges rather than expecting the dead-mandate flag to fire yet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'Is this constraint the autonomy-primacy instantiation of the one_country_two_systems_framework kernel rather than the sovereignty-primacy or balanced-coexistence instantiation, and what would a sibling adoption change structurally?',
    'Reading adoption is a committer act, not an empirical finding: resolve by observing which adjudication locus a party accepts - an external treaty forum (this reading), comprehensive central jurisdiction (sovereignty_primacy_reading), or negotiated political accommodation (balanced_coexistence_reading) - and reclassifying under that sibling''s constraint file.',
    'Under sovereignty_primacy_reading the victim set expands toward nearly all Hong Kong residents (delegated autonomy revocable at will) and central intervention ceases to count as violation; under balanced_coexistence_reading epsilon tracks the shifting negotiation balance rather than a fixed guarantee and the classification oscillates instead of settling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: this story is one of three readings of the 1C2S kernel; the disagreement is located at the enforceability locus.').

omega_variable(
    prc_net_directionality_ambiguity,
    'Does the prc_central_authorities seat experience the binding autonomy guarantee as net benefit (derived directionality near the beneficiary end) or net cost (its revealed conduct suggests otherwise)?',
    'Revealed-preference analysis: escalating interpretation activity, the 2021 white papers asserting comprehensive jurisdiction, and direct imposition of security legislation all signal net constraint cost; weigh against the prosperity and gateway benefits the centre would forfeit under framework collapse.',
    'If the seat computes near the target end, the agenda-setter seat itself bears high effective extraction and the per-seat classification sharpens toward pure extraction at that seat; if near the beneficiary end, the hybrid transitional reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prc_net_directionality_ambiguity, conceptual, 'Whether the centre is net gainer or net bearer of the binding guarantee.').

omega_variable(
    international_enforceability_gap,
    'Does any operational mechanism enforce the Joint Declaration''s guarantees, or is ''internationally enforceable'' visibility without sanction?',
    'Enumerate enforcement channels: UN registration produces reporting obligations only; no compulsory jurisdiction exists; unilateral sanctions respond to violations after the fact without restoring the guarantee. Confirm no channel compels performance.',
    'If unenforceable, the guarantee component of this reading is aspirational; the constraint''s effective burden on civic actors rises and the transitional structure decays faster toward inertial maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_enforceability_gap, empirical, 'Existence of any operational enforcement channel for the treaty guarantees.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (legal coercion, prosecution, vetting) or internalized (persistent self-censorship that survives removal of the barriers)?',
    'Post-exit suppression trajectory: emigrant surveys already report persistent self-censorship among Hongkongers living abroad; if self-censorship persists years after physical exit, a substantial internalized component is confirmed.',
    'If partially internalized, effective suppression exceeds the structural measure and would persist even under formal liberalization - classification becomes insensitive to surface legal relaxation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism split.').

omega_variable(
    suffrage_pathway_liveness,
    'Do the Basic Law Articles 45 and 68 ultimate-aim reform pathways remain live under the post-2021 candidacy-vetting settlement, or are they foreclosed in practice?',
    'Observe whether any future electoral package restores genuine competitiveness without prior patriotic-vetting foreclosure; track how central bodies treat locally initiated reform proposals.',
    'A live pathway sustains the transitional character of the arrangement; a dead pathway converts it toward theatrical maintenance of a promise nobody can redeem - driving the obsolescence verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffrage_pathway_liveness, empirical, 'Liveness of the constitutional reform destination.').

omega_variable(
    terminal_date_meaning,
    'What does the 2047 expiry of the fifty-year term mean: renewal by negotiation, absorption into the mainland system, or open-ended extension?',
    'Preference-dependent: watch for central statements on post-2047 arrangements, capital-market contracting horizons (land leases, financing tenors), and emigration timing patterns as 2047 approaches.',
    'Renewal-by-negotiation preserves the transitional frame; absorption confirms the sovereignty-primacy endpoint; extension without renegotiation freezes the current settlement indefinitely and hardens the inertial outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(terminal_date_meaning, preference, 'Meaning of the 2047 terminal date for the arrangement''s end state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 1984, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1984, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1984, 0.1).
narrative_ontology:measurement(one__tr_t1990, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1997, 0.14).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2014, 0.34).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2020, 0.47).
narrative_ontology:measurement(one__tr_t2022, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2022, 0.53).
narrative_ontology:measurement(one__tr_t2026, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2026, 0.56).

% Extraction over time
narrative_ontology:measurement(one__be_t1984, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1984, 0.14).
narrative_ontology:measurement(one__be_t1990, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1990, 0.16).
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1997, 0.19).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2003, 0.26).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2014, 0.41).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2019, 0.49).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(one__be_t2022, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2022, 0.63).
narrative_ontology:measurement(one__be_t2026, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2026, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1984, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1984, 0.1).
narrative_ontology:measurement(one__su_t1990, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1997, 0.14).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2003, 0.24).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2014, 0.42).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(one__su_t2022, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2022, 0.73).
narrative_ontology:measurement(one__su_t2026, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2026, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'One Country, Two Systems' covers three structurally distinct claims and is decomposed into a three-story constraint family sharing one kernel. Epsilon differs by construction: this autonomy-primacy story authors epsilon over the standing arrangement assessed by treaty-guarantee lights (burden concentrated on civic actors and institutional independence); the sovereignty-primacy sibling authors epsilon over the same arrangement assessed by sovereign-delegation lights (near-zero at the centre, burden shifted onto residents as revoked delegates); the balanced-coexistence sibling authors epsilon as a function of the prevailing negotiation balance. Upstream/downstream: the sovereignty-primacy reading currently governs actual practice and therefore influences the operating environment of both rivals without resolving the dispute; this reading and the coexistence reading remain competing live positions. Each family member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
