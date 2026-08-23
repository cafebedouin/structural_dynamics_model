% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__retrospective_snare_exposure, []).

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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Te Tiriti Textual Divergence as Covert Extraction (Retrospective Snare Exposure Reading)
 *   domain: constitutional law/indigenous rights/colonial history
 *
 * SUMMARY:
 *   In 1840 the Crown and Māori rangatira signed two texts that do not say
 *   the same thing. The English text cedes 'all the rights and powers of
 *   sovereignty'; the Māori text grants 'kāwanatanga' — a governor's
 *   authority — while guaranteeing 'tino rangatiratanga', unqualified
 *   chieftainship, over lands, villages, and taonga. This story authors one
 *   reading of that arrangement: the divergence between the texts is not a
 *   defect adjacent to what followed but the operative mechanism of it.
 *   Chiefs who signed the Māori text were never presented with the
 *   sovereignty claim; the assent the Crown later cited was assent to a
 *   different document. The standing arrangement — Crown authority resting on
 *   the cession claim, and the land-transfer and legislative machinery it
 *   licensed — is assessed by this reading's own lights, and ε's referent is
 *   that standing arrangement under contest (the cession-based authority and
 *   its land-transfer consequences), never the partnership or redress
 *   arrangement any reading would put in its place. The mechanism was covert
 *   at operation and became visible only retrospectively, through a century
 *   and a half of scholarship and the Tribunal's historical inquiries;
 *   visibility changed the arrangement's cover, not its structure.
 *
 * KEY AGENTS:
 *   - settler_colonial_government: agenda-setter (institutional/mobile) — asserts the cession claim, wrote the land-conversion statutes, administers settlements today
 *   - crown_land_purchase_apparatus: primary beneficiary (institutional/mobile) — received the land and the revenue through the preemption monopoly
 *   - european_settlers: secondary beneficiary (organized/mobile) — received farmland and a colony funded by land revenue
 *   - maori_signatories: primary target (organized/trapped) — rangatira who signed a text that did not contain the sovereignty claim
 *   - maori_descendant_communities: primary target, present-day (organized/identity_locked) — inherit the land loss and the constitutional relationship
 *   - maori_non_signatories: excluded (organized/trapped) — recorded refusal overridden by a general cession claim
 *   - waitangi_tribunal: analytical observer (institutional/analytical) — sees the full record; recommends, does not bind
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.88).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.78).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Te Tiriti Textual Divergence as Covert Extraction (Retrospective Snare Exposure Reading)").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional law/indigenous rights/colonial history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, 'eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904').
narrative_ontology:cs_kernel_codification('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', fixed_text).
narrative_ontology:cs_authority_grounding('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', lineage).
narrative_ontology:cs_interpretation_layer_present('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904').
narrative_ontology:cs_reading_relation('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', treaty_authority_cession__rangatiratanga_retention_reading, influences).
narrative_ontology:cs_axiom('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', foundational, no_valid_assent_without_comprehensible_text).
narrative_ontology:cs_axiom_status(no_valid_assent_without_comprehensible_text, holdable).
narrative_ontology:cs_axiom_grounding('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', no_valid_assent_without_comprehensible_text, deontological).
narrative_ontology:cs_axiom('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', foundational, textual_divergence_constitutes_mechanism).
narrative_ontology:cs_axiom_status(textual_divergence_constitutes_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', textual_divergence_constitutes_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', maori_text_limited_governance_frame).
narrative_ontology:cs_drift_state('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', contemporary_post_tribunal_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('eb97feb0-6fa3-4e58-aa7c-7dde8c4a1904', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_colonial_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchase_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, european_settlers).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendant_communities).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_non_signatories).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, crown_cession_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, crown_derived_land_title_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the governing authority the English treaty text describes as ceded. Wrote and administered the statutes under which Māori land was converted, individualized, and transferred, and today administers the settlement process that closes historical claims. It defines which text and which reading of 1840 carries legal weight, collected land revenue and authority for over a century, and in the modern period pays negotiated compensation from general funds while retaining the authority originally claimed. It can amend the governing statutes; exiting the arrangement entirely would mean re-founding the state's constitutional basis, which is formally within its power though politically enormous.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_colonial_government, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, settler_colonial_government, beneficiary).

% The Crown's land purchase and settlement administration. It held the sole right of buying Māori land, bought at administered prices rather than negotiated ones, and on-sold to settlers at a margin that funded the colony's roads, schools, and immigration. It received the bulk of the land that changed hands between 1840 and the twentieth century together with the revenue from it. Its function could be wound up without existential cost to the officials inside it; they could move to other roles in the colonial and later state service.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchase_apparatus, beneficiary,
    institutional, biographical, mobile, national).

% Received farmland at prices set by the Crown monopoly rather than by Māori sellers, plus the legal order and infrastructure funded by land revenue. Individual settlers arrived, took up Crown-granted title, built farms and towns on it, and could sell and move on; nothing bound any individual settler to the specific arrangement once title was in hand.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, european_settlers, beneficiary,
    organized, biographical, mobile, national).

% The rangatira who signed the Māori-language text in 1840, granting what they understood as a governor's limited authority while the same document guaranteed their unqualified chieftainship over their lands, villages, and treasures. The English text read the same occasion as full cession of sovereignty, but no Māori text containing that claim was put to them. After signing, the documents were held by the Crown, refusal or reversal meant war, and the Crown cited the signatures as assent to the sovereignty claim. They spoke for descendants who inherit the signed relationship.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories, payer,
    organized, generational, trapped, national).

% Inherit the arrangement: whakapapa binds them to the lands, waters, and authority their tīpuna held, and the treaty relationship is part of their constitutional identity. They bear the present-day consequences — a small retained land base, displaced authority, and settlement processes that close claims permanently in exchange for a small fraction of the value lost. There is no exit from the arrangement that leaves the person and the people intact; leaving would mean abandoning whakapapa itself.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendant_communities, payer,
    organized, generational, identity_locked, national).

% Rangatira and communities who refused to sign in 1840; their refusal is recorded. The Crown's cession claim was asserted over them anyway, and the wars, land courts, and legislation of the following decades were applied to them as if they had assented. They objected to the arrangement's foundation at the outset and were overridden by a general claim of consent they never gave.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_non_signatories, excluded,
    organized, generational, trapped, national).

% A standing commission of inquiry that hears historical claims, commissions research, and reports on the meaning and breach of the treaty texts. It sees the full record — both texts, the drafting history, the land transfers, the wars and confiscations — and its reports are the principal public record in which the 1840 divergence and its consequences are laid out. It recommends; it does not bind the government that established it.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchase_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a single framework for relations between Māori authorities and the incoming settler population: one point of contact for protection against rival imperial powers and unruly settlers, one framework for land transactions through Crown preemption, and a common instrument both sides could cite. Stated without evaluation: whatever it coordinated, the two texts signed named different things.
% TRANSFER_FUNCTION: Moves land — from effective Māori possession of the whole country in 1840 to a small retained fraction a century later — from iwi and hapū to the Crown and onward to settlers; moves governing authority from rangatira to the Crown; and in the modern period moves capped compensation from the Crown to claimant groups in exchange for permanent extinguishment of claims. On this reading, the first two transfers ran on assent given to a different text than the one later cited to license them.
% ABSENT_VOICES: The signatories' own understanding of what they granted — recoverable through oral tradition and missionary testimony — was absent when the English text's meaning was fixed in law; Wi Parata (1877) declared the treaty a 'simple nullity' with no Māori argument on the texts before the court. Non-signing rangatira's recorded refusal was overridden by a general claim of assent. Today, descendant communities hold no seat where the full-and-final character of settlements is set: the Crown defines the process, the fiscal envelope, and the extinguishment.
% DISAPPEARANCE_RATIONALE: If the cession claim and the machinery it licensed vanished overnight, the state's constitutional foundation would be void: every Crown-derived land title — the basis of the entire title system — would be clouded, the settlements architecture would lose its object, and the constitutional question of 1840 would reopen in full. The world rearranges because the arrangement is the foundation the state stands on, not a fixture on top of it.
% FOUNDING_PROBLEM: The arrangement was built to solve the Crown's problem of acquiring Māori land and authority under a recognized legal form: controlling disorderly private land speculation (the New Zealand Company's chaos), securing a single instrument of assent for annexation, and giving settlers a lawful title chain. The protective promises to Māori were part of the package presented to obtain signatures.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested from outside the benefiting parties: Waitangi Tribunal historical reports (notably the Te Paparahi o Te Raki inquiry) document the acquisition purpose and the texts' divergence from the Crown's own commissioned research; independent historiography — Ruth Ross's 1972 analysis of the translation and Claudia Orange's history of the treaty — corroborates that chiefs were not presented with the sovereignty claim; and the 1835 He Whakaputanga, in which rangatira asserted independent authority, plus Māori oral testimony, corroborates what the signatories understood themselves to be doing. The benefiting parties' own attestation — that the treaty founded a lawful cession — is precisely the account this reading contests.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.88 at interval end): by this reading's lights the arrangement moved nearly all Māori land — from effective possession of the whole country to a few percent retained — and displaced the governing authority the Māori text guaranteed, so nearly everything the cession claim licensed was taken. Suppression is 0.78 and is authored as a raw structural property, unscaled by scope or directionality — only extractiveness is scaled by the engine: war and invasion in the 1860s, confiscations, the Native Land Court's individualization of title (which dissolved the collective capacity that resistance depended on), legislative and judicial override (Wi Parata's 'simple nullity'), and modern procedural containment through a Crown-controlled claims process with full-and-final clauses. Theater is 0.62 at end and traces a dip-and-rise: high at signing (the consent ceremony presented assent to something the signatories never saw), lowest during the era of overt force and raw court-driven transfer, rising from 1975 as visibility forced re-legitimation through 'principles' and partnership rhetoric that now dominates the arrangement's activity relative to actual return of land or authority. Accessibility collapse is 0.62: alternatives partly existed at signing (some rangatira refused; He Whakaputanga 1835 asserted independent authority) but closed after it — no exit from a signed-and-held document, no alternative authority once the claim was asserted, and retrospective understanding restores nothing because title has run through generations of Crown grants to third parties. Resistance is 0.7: the New Zealand Wars, the Kīngitanga and Kotahitanga movements, Parihaka's passive resistance, the 1975 land march, Bastion Point, and five decades of claims — sustained, and partly defeated by the mechanism's individualization of title, which is itself evidence of the coalition power the arrangement had to dismantle. Claim and metrics are authored independently: 'snare' is my structural judgment (a protection-and-order cover story over a mechanism that operated through manufactured assent, held by coercion, with an identifiable and named victim set); the metric values are descriptive of the operation as the record shows it.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats should compute as different types from the same structure. From the government's seat the arrangement is the lawful foundation of the state and the settlement process is an act of redress; from the signatory seat it is an assent that was never given, held by war and statute; from the descendant seat it is a permanent closure of claims for a fraction of value, experienced as constitutional identity rather than contract; from the apparatus's seat it was a successful purchasing operation. The Tribunal seat sees the whole record and can only recommend. The engine computes per-seat classification from the structural data; the authored claim does not adjudicate between these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared beneficiaries — government, purchase apparatus, settlers — sit at the beneficiary end: the arrangement subsidized them with land, revenue, and authority, and their exits are mobile, damping their effective extraction toward zero or inversion. The declared victims — signatories, descendants, non-signatories — sit at the target end: signatories and non-signatories are trapped (no alternative authority, no exit from the asserted claim), and descendants are identity_locked (whakapapa and the treaty relationship are constitutive, so effective extraction sits near the full-target value and cannot be exited around). Spatial scope is national, and the mechanism's operation took roughly 135 years and a standing commission of inquiry to verify — the engine's scope amplification applies to the trapped and identity-locked target seats. The non-signatory seat is the sharpest asymmetry in the story: agents who explicitly withheld assent bear the arrangement in full.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — orderly colonization and land acquisition under a recognized legal form — is dead: the acquisition was substantially complete by the early twentieth century. The world still rearranges around the arrangement because the state's constitutional foundation rests on it. That status=dead × world_rearranges mismatch is the capture/zombie signature, cross-checked here against a named capture seat (the land-purchase apparatus received the gains) and rising theater — a snare with capture, not a piton (no diffuse gains, and the cost-to-fix is prohibitive precisely because the foundation would have to be re-laid). The snare classification prevents two mislabels: it is not a tangled_rope, because no shared coordination benefit runs through the same structure that took — the protection promised was not delivered on the terms claimed (the wars and confiscations followed the guarantee); and it is not a scaffold, because the settlements have no sunset into a successor arrangement — full-and-final clauses extinguish claims rather than transition them, so retrospective visibility has re-instrumentalized rather than retired the mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading (retrospective_snare_exposure) of the kernel treaty_authority_cession. What would change structurally if a sibling reading were instantiated instead?',
    'Author crown_cession_reading and rangatiratanga_retention_reading as separate stories over the same referent; compare ε, beneficiary/victim structure, and computed type across the kernel family via network links.',
    'crown_cession_reading would author low ε with no victim set (lawful cession, no one pays); rangatiratanga_retention_reading would author a contested partnership structure requiring ongoing consent. The disagreement is located in the semantic content of kāwanatanga, which text controls, and the consequence of the divergence — not in the historical facts the texts differ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; sibling readings are structurally different constraints.').

omega_variable(
    divergence_load_bearing,
    'Was the textual divergence load-bearing — the mechanism itself — in the land-transfer and authority outcomes, or a drafting defect that later transfer machinery merely exploited?',
    'Drafting-history analysis (Busby''s draft, Hobson''s instructions, Williams''s translation choices), contemporary testimony of what chiefs were told by missionaries and officials, and counterfactual analysis of whether the sovereignty claim could have been presented comprehensibly and still secured signatures.',
    'If load-bearing (this reading), ε stays high and the covert-mechanism structure holds; if incidental, ε drops materially because the operative transfer mechanisms were the land court, war, and legislation rather than the text gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divergence_load_bearing, empirical, 'Whether the translation gap was the operative mechanism or incidental to what followed.').

omega_variable(
    assent_conceptual_possibility,
    'Could any presentation have secured meaningful Māori assent to indivisible sovereignty in 1840, given that mana and rangatiratanga are held rather than surrendered — or was assent structurally impossible regardless of translation quality?',
    'Comparative analysis of Māori political concepts at 1840 (whaikōrero, the He Whakaputanga assertion of independent authority) against the sovereignty concept; linguistic evidence on whether any Māori formulation could have carried the English claim.',
    'If structurally impossible, all signatures are manufactured consent regardless of translation and this reading strengthens; if a faithful presentation was possible, a counterfactual valid cession exists and ε moderates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assent_conceptual_possibility, conceptual, 'Whether meaningful assent to sovereignty was conceptually possible in 1840 at all.').

omega_variable(
    settlement_architecture_trajectory,
    'Does the modern settlement architecture — negotiated redress, full-and-final clauses, Crown-controlled process — dissolve the arrangement''s transfer operation, or add a new layer to it?',
    'Track settlement outcomes against independent valuations of lost land and authority; observe whether claim-closing clauses hold against reasserted claims and whether co-governance arrangements transfer real authority or ceremonial recognition.',
    'If settlements are genuine transition, the arrangement may acquire a sunset character with falling ε; if they re-instrumentalize (capped compensation extinguishing claims permanently), ε stays high and the retrospective-visibility period is itself part of the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_architecture_trajectory, empirical, 'Whether retrospective visibility and the settlements dissolve or re-instrumentalize the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.55).
narrative_ontology:measurement_basis(trea_tr_t1840, observed).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1860, 0.38).
narrative_ontology:measurement_basis(trea_tr_t1860, observed).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1880, 0.3).
narrative_ontology:measurement_basis(trea_tr_t1880, observed).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.34).
narrative_ontology:measurement_basis(trea_tr_t1900, observed).
narrative_ontology:measurement(trea_tr_t1930, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1930, 0.4).
narrative_ontology:measurement_basis(trea_tr_t1930, observed).
narrative_ontology:measurement(trea_tr_t1960, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1960, 0.46).
narrative_ontology:measurement_basis(trea_tr_t1960, observed).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.5).
narrative_ontology:measurement_basis(trea_tr_t1975, observed).
narrative_ontology:measurement(trea_tr_t1990, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1990, 0.56).
narrative_ontology:measurement_basis(trea_tr_t1990, observed).
narrative_ontology:measurement(trea_tr_t2010, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2010, 0.6).
narrative_ontology:measurement_basis(trea_tr_t2010, observed).
narrative_ontology:measurement(trea_tr_t2026, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2026, 0.62).
narrative_ontology:measurement_basis(trea_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.55).
narrative_ontology:measurement_basis(trea_be_t1840, observed).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1860, 0.68).
narrative_ontology:measurement_basis(trea_be_t1860, observed).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1880, 0.82).
narrative_ontology:measurement_basis(trea_be_t1880, observed).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.88).
narrative_ontology:measurement_basis(trea_be_t1900, observed).
narrative_ontology:measurement(trea_be_t1930, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1930, 0.9).
narrative_ontology:measurement_basis(trea_be_t1930, observed).
narrative_ontology:measurement(trea_be_t1960, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1960, 0.88).
narrative_ontology:measurement_basis(trea_be_t1960, observed).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.86).
narrative_ontology:measurement_basis(trea_be_t1975, observed).
narrative_ontology:measurement(trea_be_t1990, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1990, 0.84).
narrative_ontology:measurement_basis(trea_be_t1990, observed).
narrative_ontology:measurement(trea_be_t2010, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2010, 0.86).
narrative_ontology:measurement_basis(trea_be_t2010, observed).
narrative_ontology:measurement(trea_be_t2026, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2026, 0.88).
narrative_ontology:measurement_basis(trea_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement_basis(trea_su_t1840, observed).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1860, 0.6).
narrative_ontology:measurement_basis(trea_su_t1860, observed).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1880, 0.75).
narrative_ontology:measurement_basis(trea_su_t1880, observed).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement_basis(trea_su_t1900, observed).
narrative_ontology:measurement(trea_su_t1930, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1930, 0.78).
narrative_ontology:measurement_basis(trea_su_t1930, observed).
narrative_ontology:measurement(trea_su_t1960, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement_basis(trea_su_t1960, observed).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.66).
narrative_ontology:measurement_basis(trea_su_t1975, observed).
narrative_ontology:measurement(trea_su_t1990, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement_basis(trea_su_t1990, observed).
narrative_ontology:measurement(trea_su_t2010, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement_basis(trea_su_t2010, observed).
narrative_ontology:measurement(trea_su_t2026, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2026, 0.78).
narrative_ontology:measurement_basis(trea_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, he_whakaputanga_1835).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the Treaty of Waitangi' decomposes into three structurally distinct constraints — one per reading of the kernel treaty_authority_cession. crown_cession_reading authors a lawful-foundation constraint with negligible extraction; rangatiratanga_retention_reading authors a contested partnership constraint; this story authors the covert-mechanism constraint with high ε over the same referent. The upstream sibling (crown_cession_reading) is the account the benefiting parties cite as evidence; this reading's Tribunal-era record is the evidentiary base on which the retention reading's institutional form (principles, partnership jurisprudence) was built, hence the influences edge. Each file carries its own ε, stakeholders, and classification; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
